module TacLua where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans.State
import AbsLua
--import PrintC
import ErrM
import Debug.Trace

type TacInst=[TAC]

data TacM = TacM{
				kaddr::Int,
				klab::Int,
				code::TacInst}

startState = TacM 0 0 []

data Env =Env [BlockEnv]  deriving (Eq, Ord, Show, Read)

data BlockEnv = BlockEnv {
  funDefs::Sigs,
  varDefs::Context,
  blockTyp::BlockTyp
}
 deriving (Eq, Ord, Show, Read)

data BlockTyp = BTroot | BTdecs | BTcomp | BTloop | BTifEls | BTfun Typ
  deriving (Eq, Ord, Show, Read)

type Ident = String
type Typ = Type_specifier
type Addr= Int
type Label = Int
type Mod = Maybe Modality

type Sigs =Map.Map Ident PosSig
type Context = Map.Map Ident PosTypMod

type Sig = (Typ,[TypMod],Maybe Label) --cambiare ordine di typmod e typ
type Pos = (Int,Int)
type PosTyp=(Pos,Typ)
type PosTypMod = (Pos,TypMod,Maybe Addr)
type TypMod = (Typ,Mod)
type PosSig = (Pos,Sig)

data TAC= TACAssign String String
	| TACBinaryOp Int Int InfixOp Int
	| TACBinaryArithOp Int Int ArithOp Int
	| TACUnaryOp Int Unary_Op Int
	| TACWhile String String String
	| TACIf TAC String String
	| TACCondition String String String
	| TACReturn String
	| TACCall String String String
	| TACLabel Label
	| TACParam String
	| TACPreamble String
	| TACGoto String
	| TACInit Ident Exp
	| TACInt Int
	| TACBool Bool
	| TACChar Char
	| TACString String
	| TACFloat Float
 deriving(Show)

--TODO:riscrivere funzioni per la dichiarazione di funzione,
--capire come gestire gli array e capire cosa viene fatto nelle infix operations.Il codice che viene
--scritto in addparam dovrebbe essere spostato in un'altra funzione del tac.
-------------------------------
--funzioni ausiliarie per ENV--
-------------------------------
addDec::Env->Dec->State TacM(Env)  --TODO:quando faccio codeFunc DEVO ricordarmi di aggiungere i temporanei per i parametri!!!
addDec env@(Env (current:stack)) dec = case dec of
  VarDeclar typ pident@(Pident (pos,ident)) exp ->
  	case exp of 
        Just exp-> do
            isexpsimp<-issimple exp
            case isexpsimp of
               True->do
                newBlockEnv<-addVarDec current ident pos typ Nothing Nothing
                return (Env (newBlockEnv:stack))
               False->do
                tmp<-newtemp
                newBlockEnv<-addVarDec current ident pos typ Nothing (Just tmp)
                return (Env (newBlockEnv:stack))
        Nothing -> do
            newBlockEnv<-addVarDec current ident pos typ Nothing Nothing
            return (Env (newBlockEnv:stack))
  Func typ pident@(Pident (pos,ident)) params nparams _  -> do
    label<-newlabel
    newBlockEnv<-addFuncDec current ident pos typ (getParamsModTyp params) (Just label)
    return (Env (newBlockEnv:stack))
--aggiunge una variabile a un contesto
addVarDec::BlockEnv->Ident->Pos->Typ->Mod->Maybe Addr->State TacM(BlockEnv)
addVarDec (BlockEnv sigs context blockTyp) ident pos@(line,col) typ mod tmp= do
  record<-lookVarInContext ident context
  case record of
    Nothing -> return (BlockEnv sigs (Map.insert ident (pos,(typ,mod),tmp) context) blockTyp)
    --Just (pos',_) -> fail $ (show pos) ++ ": variable "++ ident ++ " already declared in " ++ (show pos')

--aggiunge una funzione a un contesto
addFuncDec::BlockEnv->Ident->Pos->Typ->[(Typ,Mod)]->Maybe Label->State TacM(BlockEnv)
addFuncDec (BlockEnv sigs context blockTyp)  ident pos@(line,col) returnTyp paramsTyp label = do
  record<-lookFuncInSigs ident sigs
  --tmps<-take (length paramsTyp) tmp
  case record of
    Nothing -> return (BlockEnv (Map.insert ident (pos,(returnTyp,paramsTyp,label)) sigs) context  blockTyp)
    --Just (pos',_) -> fail $ (show pos) ++ ": function "++ ident ++ " already declared in " ++ (show pos')

addParams::Env->[Argument]->State TacM(Env)
addParams env parameters = foldM addParam env parameters
 
addParam::Env->Argument->State TacM(Env) --riscrivere in modo che
addParam (Env (current:stack)) pars = case pars of
    FormPar mod typ (Pident (pos,ident)) -> do
      tmp<-newtemp
      newBlockEnv<-addVarDec current ident pos typ (Just mod) (Just tmp)
      --addCode $ tmp ++ " = " ++ ident --idealmente questo va separato
      --addTAC $ TACAssign tmp ident
      return (Env (newBlockEnv:stack))


emptyEnv = Env [emptyBlockEnv BTroot]
emptyBlockEnv blockTyp = BlockEnv Map.empty Map.empty blockTyp
newBlockEnv::BlockTyp->BlockEnv
newBlockEnv blockTyp = BlockEnv Map.empty Map.empty blockTyp

pushNewBlocktoEnv::Env->BlockTyp->State TacM(Env)
pushNewBlocktoEnv (Env blocks) blocktyp= return $ Env ((newBlockEnv blocktyp):blocks)

--HELPERS--

getParamsModTyp::[Argument]->[(Typ,Mod)]
getParamsModTyp params = map (\(FormPar mod typ _)->(typ,Just mod)) params

lookVar::Pident->Env->State TacM(PosTypMod)
lookVar pident@(Pident (pos,ident)) (Env stack) = case stack of
  --[] -> fail $ (show pos) ++ ": variable " ++ (show ident) ++ " out of scope"
  (current@(BlockEnv _ context _ ):parent) -> do
    maybePosTyp <- lookVarInContext ident context
    case maybePosTyp of
      Nothing -> lookVar pident (Env parent)
      Just posTyp-> return posTyp

lookFunc::Pident->Env->State TacM(PosSig)
lookFunc pident@(Pident (pos,ident)) (Env stack) = case stack of
  --[] -> fail $ (show pos) ++ ": function " ++ (show ident) ++ " out of scope"
  (current@(BlockEnv sigs _ _ ):parent) -> do
    maybePosTyp <- lookFuncInSigs ident sigs
    case maybePosTyp of
      Nothing -> lookFunc pident (Env parent)
      Just posTyp-> return posTyp


lookVarInContext::Ident->Context->State TacM(Maybe PosTypMod)
lookVarInContext ident context= do
  return (Map.lookup ident context)

lookFuncInSigs::Ident->Sigs->State TacM(Maybe PosSig)
lookFuncInSigs ident sigs= do
  return (Map.lookup ident sigs)


createInitialEnv::Env->State TacM(Env)
createInitialEnv (Env (current:stack)) = do
  --label <- newlabel
  newBlockEnv<-addFuncDec current "writeInt" (-1,-1) Tvoid [(Tint,Just Modality_VAL)] Nothing
  --label1 <- newlabel
  newBlockEnv<-addFuncDec newBlockEnv "readInt" (-1,-1)  Tint [] Nothing
  return (Env ((emptyBlockEnv BTdecs):newBlockEnv:stack))

filterdecs::[Dec]->[Dec]
filterdecs []=[]

filterdecs (dec:decs)= case dec of
  Func _ _ _ _ _ ->dec: filterdecs decs
  otherwise->filterdecs decs

issimple::Exp->State TacM(Bool)
issimple exp= case exp of
	Eint _-> return True
	Ebool _-> return True
	Estring _-> return True
	Efloat _-> return True
	Echar _-> return True
	otherwise->return False

--generatori di temporanei e label--

newtemp ::State TacM (Addr)
newtemp = do
          c<-gets kaddr
          modify $ \s->s{kaddr=c+1}
          return c

newlabel ::State TacM (Label)
newlabel = do 
          l<-gets klab
          modify $ \s->s{klab=l+1}
          return l
-------------------------------------
------funzioni per gestione TAC------
-------------------------------------
{--addCode :: String -> State TacM()
addCode newCode = modify (\attr -> attr{code = (code attr) ++ newCode ++ "\n"})
--}
addTAC :: [TAC] -> State TacM()
addTAC nxtinst = do
    modify (\attr -> attr{code = (code attr) ++ nxtinst})
    return ()
addTAC1 :: TAC -> State TacM() --da eliminare ma lo usiamo per il momento
addTAC1 nxtinst = do
    modify (\attr -> attr{code = (code attr) ++ [nxtinst]})
    return ()

tacGenerator program = execState (codeProgram program) startState

codeProgram :: Program -> State TacM ()
codeProgram (Progr decls) = do
    label <- newlabel
    env<-createInitialEnv emptyEnv
    codeDecls env decls
    {--addCode $ label ++ ":halt"
    addTAC $ TACLabel label
    addTAC $ TACPreamble "halt"--}
    return ()

codeDecls :: Env->[Dec] -> State TacM(Env)
codeDecls env decs = do 
		newEnv<-foldM addDec env fundecs
		foldM codeDecl env decs
			where fundecs=filterdecs decs

codeDecl :: Env->Dec-> State TacM (Env)
codeDecl env dec = case dec of
      VarDeclar typ (Pident(_,id)) exp -> 
        case exp of 
            Just exp-> do
              newEnv <- addDec env dec 
              (addr,codexp)<-codeexp env exp   --controllare,dovrebbe andare bene   
              --addr_RExpr <- gets addr
              --addTAC $ TACAssign (show id) addr
              --addCode $ tmp ++ "=" ++ (show id)
              --addTAC $ TACAssign tmp (show id)
              return (newEnv)
            Nothing-> do
              newEnv <- addDec env dec
              --addCode $ tmp ++ "=" ++ (show id)  --aggiungere codice di dichiarazione semplice???
              --addTAC $ TACAssign tmp (show id)
              return newEnv
              --pushTempType (tmp,(getBasicType basTyp)) --perchè mi interessa il tipo del temp?
                                 --controllato già dal tc?
      Func retTyp ident params _ decstmts -> do
          label <- newlabel
          newEnv<-addDec env dec
          pushEnv<-(pushNewBlocktoEnv newEnv (BTfun retTyp))
          pushEnv<-addParams pushEnv params --porto dentro i parametri
          --addCode $ label ++ ":"
          addTAC1 $ TACLabel label
          --addCode "BeginFunc"
          --addTAC $ TACPreamble "BeginFunc"
          --pushEnv<-code_Decls pushEnv decs --qui bisogna mettere a posto,se c'è altra def di funzione,questa va fatta dopo la fine di quella precedente
          --code_Stmts pushEnv stmts         --TODO bisogna 1)modificare env quando codifico decs e 2)gestire dec di funzione diversamente rispetto alle dec di variabili
          --addCode "EndFunc"
          --addTAC $ TACPreamble "EndFunc"
        --setOldEnv  --non dovrebbe essere necessario
          return newEnv
          --}
codeexp :: Env->Exp -> State TacM (Addr,[TAC])
codeexp env exp = case exp of 
	InfixOp op exp1 exp2  -> case op of
		ArithOp subop->codeArithOp env exp1 exp2 subop
	Unary_Op subop exp->codeUnaryOp env subop exp
	{--Eint (Pint(_,int)) -> return (int,TACInt int)--}
	RelOp subop exp1 exp2->codeRelOp env subop exp1 exp2
               

codeArithOp::Env->Exp->Exp->ArithOp->State TacM (Addr,[TAC])
codeArithOp env exp1 exp2 op=do
        (addr1,code1)<-codeexp env exp1
        (addr2,code2)<-codeexp env exp2
        addr<-newtemp
        let code=code1++code2++[TACBinaryArithOp addr addr1 op addr2]
        return(addr,code)
codeUnaryOp :: Env->Unary_Op->Exp-> State TacM (Addr,[TAC])
codeUnaryOp env op exp = do   --forse devo gestire LogNeg diversamente
    (addr1,code1)<-codeexp env exp
    addr <- newtemp
    let code=code1++[TACUnaryOp addr op addr1]
    return (addr,code)
codeRelOp::Env->BoolOp->Exp->State TacM(Addr,[TAC])
codeRelOp env op exp1 exp2= case op of
     Or->do
      (addr1,code1)<-codeexp env exp1
      (addr2,code2)<-codeexp env exp2
