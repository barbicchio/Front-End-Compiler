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
type Addr= String
type Label = String
type Mod = Maybe Modality

type Sigs =Map.Map Ident PosSig
type Context = Map.Map Ident PosTypMod

type Sig = (Typ,[TypMod],Maybe Label)
type Pos = (Int,Int)
type PosTyp=(Pos,Typ)
type PosTypMod = (Pos,TypMod,Maybe Addr)
type TypMod = (Typ,Mod)
type PosSig = (Pos,Sig)

data TAC= TACAssign String String
	| TACBinaryOp String String String String
	| TACUnaryOp String String String
	| TACWhile String String String
	| TACIf TAC String String
	| TACCondition String String String
	| TACReturn String
	| TACCall String String String
	| TACLabel String
	| TACParam String
	| TACPreamble String
	| TACGoto String
 deriving(Show)

--TODO:riscrivere funzioni per la dichiarazione di funzione,
--capire come gestire gli array e capire cosa viene fatto nelle infix operations.Il codice che viene
--scritto in addparam dovrebbe essere spostato in un'altra funzione del tac.
-------------------------------
--funzioni ausiliarie per ENV--
-------------------------------
addDec::Env->Dec->Maybe String->State TacM(Env)  --TODO:quando faccio codeFunc DEVO ricordarmi di aggiungere i temporanei per i parametri!!!
addDec env@(Env (current:stack)) dec tol  = case dec of
  VarDeclar typ pident@(Pident (pos,ident)) exp -> do
    --tmp<-newtemp
    newBlockEnv<-addVarDec current ident pos typ Nothing tol
    return (Env (newBlockEnv:stack))
  Func typ pident@(Pident (pos,ident)) params nparams _  -> do
    --label<-newlabel
    newBlockEnv<-addFuncDec current ident pos typ (getParamsModTyp params) tol
    return (Env (newBlockEnv:stack))
--aggiunge una variabile a un contesto
addVarDec::BlockEnv->Ident->Pos->Typ->Mod->Maybe Addr->State TacM(BlockEnv)
addVarDec (BlockEnv sigs context blockTyp) ident pos@(line,col) typ mod tmp = do
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
  label <- newlabel
  newBlockEnv<-addFuncDec current "writeInt" (-1,-1) Tvoid [(Tint,Just Modality_VAL)] (Just label)
  label1 <- newlabel
  newBlockEnv<-addFuncDec newBlockEnv "readInt" (-1,-1)  Tint [] (Just label)
  return (Env ((emptyBlockEnv BTdecs):newBlockEnv:stack))

--generatori di temporanei e label--

newtemp ::State TacM (Addr)
newtemp = do
          c<-gets kaddr
          modify $ \s->s{kaddr=c+1}
          return $ "t@"++ show c

newlabel ::State TacM (Label)
newlabel = do 
          l<-gets klab
          modify $ \s->s{klab=l+1}
          return $ "lab" ++ show l
-------------------------------------
------funzioni per gestione TAC------
-------------------------------------
{--addCode :: String -> State TacM()
addCode newCode = modify (\attr -> attr{code = (code attr) ++ newCode ++ "\n"})
--}
addTAC :: TAC -> State TacM()
addTAC nxtinst = do
    modify (\attr -> attr{tac = (tac attr) ++ [nxtinst]})
    return ()
--}

tacGenerator program = execState (code_Program program) startState

code_Program :: Program -> State TacM ()
code_Program (Progr decls) = do
    label <- newlabel
    env<-createInitialEnv emptyEnv
    --code_PredefinedFuncs predefinedFuncs   --per il momento non genero codice per le funzioni predefinite,
                         --capire se bisogna farlo o meno
    code_Decls env decls      --mi porto dietro l'env
    --addCode $ label ++ ":halt"
    --addTAC $ TACLabel label
    --addTAC $ TACPreamble "halt"
    return ()

code_Decls :: Env->[Dec] -> State TacM(Env)
code_Decls env decs = foldM code_Decl env decs

code_Decl :: Env->Dec-> State TacM (Env)
code_Decl env dec = case dec of
      VarDeclar typ (Pident(_,id)) exp -> 
        case exp of 
            Just exp-> do
              tmp<-newtemp
              (addr,codexp)<-code_exp env exp   --controllare,dovrebbe andare bene
              newEnv <- addDec env dec (Just tmp)     
              --addr_RExpr <- gets addr
              --pushVarToEnv (name,tmp)   NO
              --addTAC $ TACAssign (show id) addr
              --addCode $ tmp ++ "=" ++ (show id)
              --addTAC $ TACAssign tmp (show id)
              return (newEnv)
           {-- Nothing-> do
              --tmp<-newtemp
              newEnv <- addDec env dec Nothing
              --addCode $ tmp ++ "=" ++ (show id)  --aggiungere codice di dichiarazione semplice???
              --addTAC $ TACAssign tmp (show id)
              return newEnv
              --pushTempType (tmp,(getBasicType basTyp)) --perchè mi interessa il tipo del temp?
                                 --controllato già dal tc?--}
      Func retTyp ident params _ decstmts -> do
          label <- newlabel
          newEnv<-addDec env dec (Just label)
          pushEnv<-(pushNewBlocktoEnv newEnv (BTfun retTyp))
          pushEnv<-addParams pushEnv params --porto dentro i parametri
          --addCode $ label ++ ":"
          addTAC $ TACLabel label
          --addCode "BeginFunc"
          --addTAC $ TACPreamble "BeginFunc"
          --pushEnv<-code_Decls pushEnv decs --qui bisogna mettere a posto,se c'è altra def di funzione,questa va fatta dopo la fine di quella precedente
          --code_Stmts pushEnv stmts         --TODO bisogna 1)modificare env quando codifico decs e 2)gestire dec di funzione diversamente rispetto alle dec di variabili
          --addCode "EndFunc"
          --addTAC $ TACPreamble "EndFunc"
        --setOldEnv  --non dovrebbe essere necessario
          return newEnv
          --}
code_exp :: Env->Exp -> State TacM (Addr,[TAC])
code_exp env exp = return ("t0",[])