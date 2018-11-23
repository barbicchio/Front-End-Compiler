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
				code::TacInst,
			    tacmap::TacContext}
	deriving (Show)			

startState = TacM 0 0 [] (Map.empty)

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
type Addr= String
type Label = String
type Mod = Maybe Modality

type Sigs =Map.Map Ident PosSig
type Context = Map.Map Ident PosTypMod
type TacContext = Map.Map (Ident,Pos) Addr

type Sig = (Typ,[TypMod],Maybe Label) --cambiare ordine di typmod e typ
type Pos = (Int,Int)
type PosTyp=(Pos,Typ)
type PosTypMod = (Pos,TypMod)
type TypMod = (Typ,Mod)
type PosSig = (Pos,Sig)

data TAC= TACAssign Addr Addr {--modificare tutto con le posizioni
	| TACBinaryOp Int Int InfixOp Int--}
	| TACBinaryArithOp Addr Addr ArithOp Addr
	| TACLabel Label
	| TACTmp Ident Pos Typ Addr
	| TACUnaryOp Addr Unary_Op Addr
	| TACNewTemp Addr Typ Ident Pos
	| TACIncrDecr Addr Addr IncrDecr
	{--| TACWhile String String String
	--| TACIf TAC String String
	--| TACCondition String String String
	--| TACReturn String
	--| TACCall String String String
	--| TACParam String
	--| TACPreamble String
	--| TACGoto String --}
	| TACInit Typ Ident Pos (Maybe String) (Maybe String)
	{--| TACInt Int
	--| TACBool Bool
	--| TACChar Char
	--| TACString String
	--| TACFloat Float --}
 deriving(Show)

--TODO:riscrivere funzioni per la dichiarazione di funzione,
--capire come gestire gli array e capire cosa viene fatto nelle infix operations.
--Quando faccio cast devo aggiungere un temporaneo
-------------------------------
--funzioni ausiliarie per ENV--
-------------------------------
addDec::Env->Dec->State TacM(Env)  --TODO:quando faccio codeFunc DEVO ricordarmi di aggiungere i temporanei per i parametri!!!
addDec env@(Env (current:stack)) dec = case dec of
  VarDeclar typ pident@(Pident (pos,ident)) exp ->do
    newBlockEnv<-addVarDec current ident pos typ Nothing
    return (Env (newBlockEnv:stack))
  Func typ pident@(Pident (pos,ident)) params nparams _  -> do
    label<-newlabel
    newBlockEnv<-addFuncDec current ident pos typ (getParamsModTyp params) (Just label)
    return (Env (newBlockEnv:stack))
--aggiunge una variabile a un contesto
addVarDec::BlockEnv->Ident->Pos->Typ->Mod->State TacM(BlockEnv)
addVarDec (BlockEnv sigs context blockTyp) ident pos@(line,col) typ mod= do
  record<-lookVarInContext ident context
  case record of
    Nothing -> return (BlockEnv sigs (Map.insert ident (pos,(typ,mod)) context) blockTyp)

--aggiunge una funzione a un contesto
addFuncDec::BlockEnv->Ident->Pos->Typ->[(Typ,Mod)]->Maybe Label->State TacM(BlockEnv)
addFuncDec (BlockEnv sigs context blockTyp)  ident pos@(line,col) returnTyp paramsTyp label = do
  record<-lookFuncInSigs ident sigs
  case record of
    Nothing -> return (BlockEnv (Map.insert ident (pos,(returnTyp,paramsTyp,label)) sigs) context blockTyp)

addParams::Env->[Argument]->State TacM(Env)
addParams env parameters = foldM addParam env parameters
 
addParam::Env->Argument->State TacM(Env) --riscrivere in modo che
addParam (Env (current:stack)) pars = case pars of
    FormPar mod typ (Pident (pos,ident)) -> do
      newBlockEnv<-addVarDec current ident pos typ (Just mod)
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
  (current@(BlockEnv _ context _ ):parent) -> do
    maybePosTyp <- lookVarInContext ident context
    case maybePosTyp of
      Nothing -> lookVar pident (Env parent)
      Just posTyp-> return posTyp

lookFunc::Pident->Env->State TacM(PosSig)
lookFunc pident@(Pident (pos,ident)) (Env stack) = case stack of
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

gettypid::Exp->State TacM(String,String)
gettypid exp=case exp of
	Eint (Pint(_,id))-> return ("Int",id)
	Ebool (Pbool(_,id))-> return ("Bool",id)
	Estring (Pstring(_,id))-> return ("String",id)
	Efloat (Preal(_,id))-> return ("Float",id)
	Echar (Pchar(_,id))-> return ("Char",id)

--generatori di temporanei e label--

newtemp ::State TacM (Addr)
newtemp = do
          c<-gets kaddr
          modify $ \s->s{kaddr=c+1}
          return $'t':(show c)

newlabel ::State TacM (Label)
newlabel = do 
          l<-gets klab
          modify $ \s->s{klab=l+1}
          return $"label"++ (show l)
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
addTmp::(Ident,Pos)->Addr->State TacM()
addTmp idpos addr= do
    modify (\s@TacM{tacmap=m} -> s{ tacmap=Map.insert idpos addr m})
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
		foldM codeDecl newEnv decs
			where fundecs=filterdecs decs

codeDecl :: (Env)->Dec-> State TacM (Env)
codeDecl env dec = case dec of
      VarDeclar typ ident@(Pident(pos,id)) exp -> 
        case exp of 
            Just exp-> do
              newEnv <- addDec env dec 
              simplexp<-issimple exp
              case simplexp of --caso inizializzazione con semplice valore,no temporaneo. 
               True->do
               	(typexp,exp)<-gettypid exp
                addTAC $ [TACInit typ id pos (Just typexp) (Just exp)]
                return newEnv  
               False->do --se inizializzazione complessa,allora temporaneo. 
               	addr<-codeexp env exp
                addTAC $ [TACTmp id pos typ addr]
                addTmp (id,pos) addr --aggiungo alla mappa la coppia (id,pos)-indirizzo
                return newEnv
            Nothing-> do --caso dichiarazione senza
              addTAC $ [TACInit typ id pos Nothing Nothing] --se possibile migliorare il tipo che viene stampato (soprattutto per gli array)
              newEnv <- addDec env dec
              return newEnv
      Func retTyp ident params _ decstmts -> do
          label <- newlabel
          newEnv<-addDec env dec
          pushEnv<-(pushNewBlocktoEnv newEnv (BTfun retTyp))
          pushEnv<-addParams pushEnv params --porto dentro i parametri
          --addCode $ label ++ ":"
          addTAC $ [TACLabel label]
          --addCode "BeginFunc"
          --addTAC $ TACPreamble "BeginFunc"
          --pushEnv<-code_Decls pushEnv decs --qui bisogna mettere a posto,se c'è altra def di funzione,questa va fatta dopo la fine di quella precedente
          --code_Stmts pushEnv stmts         --TODO bisogna 1)modificare env quando codifico decs e 2)gestire dec di funzione diversamente rispetto alle dec di variabili
          --addCode "EndFunc"
          --addTAC $ TACPreamble "EndFunc"
        --setOldEnv  --non dovrebbe essere necessario
          return newEnv
          --}
codeexp :: Env->Exp -> State TacM (Addr)
codeexp env exp = case exp of 
	InfixOp op exp1 exp2  -> case op of
		ArithOp subop->codeArithOp env exp1 exp2 subop
	Unary_Op subop exp->codeUnaryOp env subop exp
	PrePost prepost exp->case prepost of
		Pre op->do
			tmp<-newtemp
			addrlexp<-codelexp env exp
			addTAC $ [TACAssign tmp addrlexp]++[TACIncrDecr addrlexp tmp op]
			return addrlexp
		Post op->do
			addrlexp<-codelexp env exp
			addTAC $ [TACIncrDecr addrlexp addrlexp op]
			return addrlexp
	Eint (Pint(_,num)) -> return num
	Efloat (Preal(_,num)) -> return num
	Ebool (Pbool(_,val))->return val
	Estring (Pstring(_,string))->return string
	Echar (Pchar(_,char))->return char
	Evar ident@(Pident(_,id))->do
		(pos,(typ,_))<-lookVar ident env
		addr<-newtemp
		addTmp (id,pos) addr
		addTAC $ [TACNewTemp addr typ id pos]
		return(addr)
	otherwise->codelexp env exp
		{--tm<-gets tacmap
		let tmp=Map.lookup (id,pos) tm
		    tmp of 
			Nothing->do 
			addr<-newtemp
            addTac $ TACNewTemp addr typ id pos
            addTmp (id,pos) addr
			Just tmp->do
            addTac
         --}
	{--RelOp subop exp1 exp2->codeRelOp env subop exp1 exp2--}
codelexp::Env->Exp->State TacM(Addr)
codelexp env exp= case exp of
	Evar ident@(Pident(_,id))->do
		(pos,_)<-lookVar ident env
		let idpos=id++"_"++(show pos)
		--addTmp (id,pos) addr
		--addTAC $ [TACNewTemp addr typ id pos]
		return(idpos)
	{--Arraysel id exp->do
		(pos,(typ,_))<-lookVar id  env
--}

codeArithOp::Env->Exp->Exp->ArithOp->State TacM (Addr)
codeArithOp env exp1 exp2 op=do
        addr1<-codeexp env exp1
        addr2<-codeexp env exp2
        addr<-newtemp
        addTAC $ [TACBinaryArithOp addr addr1 op addr2]
        return(addr)
codeUnaryOp :: Env->Unary_Op->Exp-> State TacM (Addr)
codeUnaryOp env op exp = case op of
    Neg->do
      addr1<-codeexp env exp
      addr<-newtemp
      addTAC $[TACUnaryOp addr op addr1]
      return addr
--devo gestire LogNeg diversamente

{--codeRelOp::Env->BoolOp->Exp->State TacM(Addr,[TAC])
codeRelOp env op exp1 exp2= case op of
     Or->do
      (addr1,code1)<-codeexp env exp1
      (addr2,code2)<-codeexp env exp2
--}