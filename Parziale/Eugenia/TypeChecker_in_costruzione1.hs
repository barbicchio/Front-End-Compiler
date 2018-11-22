module TypeChecker where

import System.IO
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import AbsLua
import PrintLua
import ErrM
import Debug.Trace

data Env = Env [BlockEnv]
  deriving (Eq, Ord, Show, Read)

data BlockEnv = BlockEnv {
  funDefs :: Sigs,
  varDefs :: Context,
  blockTyp :: BlockTyp
} 
  deriving (Eq, Ord, Show, Read)

data BlockTyp = BTroot | BTdecs | BTcomp | BTloop | BTifEls | BTfun Typ
  deriving (Eq, Ord, Show, Read)

type Ident = String
type Typ = Type_specifier
type Mod = Maybe Modality

type Sigs = Map.Map Ident PosSig
type Context = Map.Map Ident PosTyp

type Sig = ([TypMod],Typ,Int)
type Pos = (Int,Int)
type PosTyp = (Pos,Typ)
type PosSig = (Pos,Sig)
type TypMod = (Typ,Mod)
type PosTypMod = (Pos,TypMod)

---------------------
-----TYPECHECKER-----
---------------------

typecheck :: Program -> IO ()
typecheck p@(Progr decls) = do
  env <- createInitialEnv emptyEnv
  checkDecs env decls
  return ()

checkDecs :: Env -> [Dec] -> IO Env
checkDecs env decsstats = do 
newEnv <- foldM addDec env fundecs
foldM checkDec newEnv decsstats --foldM analogo di foldl per le monadi
  where fundecs = filterdecs decsstats

filterdecs :: [Dec] -> [Dec]
filterdecs [] = []

filterdecs (dec:decs) = case dec of
  Func _ _ _ _ _ _ -> dec: filterdecs decs
  otherwise -> filterdecs decs

---------------------
----ENV FUNCTIONS----
---------------------

emptyEnv = Env [emptyBlockEnv BTroot]
emptyBlockEnv blockTyp = BlockEnv Map.empty Map.empty blockTyp
newBlockEnv :: BlockTyp -> BlockEnv
newBlockEnv blockTyp = BlockEnv Map.empty Map.empty blockTyp

pushNewBlocktoEnv :: Env -> BlockTyp -> IO Env
pushNewBlocktoEnv (Env blocks) blocktyp = return $ Env ((newBlockEnv blocktyp):blocks)

--smista i tipi di dichiarazione da aggiungere all'env e aggiunge nel contesto corrente
addDec :: Env -> Dec -> IO Env
addDec env@(Env (current:stack)) dec = case dec of
  InitDeclarI typ pident@(Pident (pos,ident)) _ -> do
    newBlockEnv <- addVarDec current ident pos typ
    return (Env (newBlockEnv:stack))
  Func typ pident@(Pident (pos,ident)) params nParams _ _ -> do
    newBlockEnv <- addFuncDec current ident pos typ (getParamsTyp params) nParams
    return (Env (newBlockEnv:stack))
  InitDeclarArr typ pident@(Pident (pos,ident)) _ -> do
    newBlockEnv <- addVarDec current ident pos (Tarray typ)
    return (Env (newBlockEnv:stack))

--aggiunge una variabile a un contesto
addVarDec :: BlockEnv -> Ident -> Pos -> Typ -> IO BlockEnv
addVarDec curr@(BlockEnv sigs context blockTyp) ident pos@(line,col) typ = do
  record <- lookVarInContext ident context
  case record of
    Nothing -> return (BlockEnv sigs (Map.insert ident (pos,typ) context ) blockTyp)
    Just (pos',_) -> do
      putStrLn $ (show pos) ++ ": variable "++ ident ++ " already declared in " ++ (show pos')
      return curr

--aggiunge una funzione a un contesto
addFuncDec :: BlockEnv -> Ident -> Pos -> Typ -> [Typ] -> Int -> IO BlockEnv
addFuncDec curr@(BlockEnv sigs context blockTyp)  ident pos@(line,col) returnTyp paramsTyps nParams = do
  record <- lookFuncInSigs ident sigs
  case record of
    Nothing -> return (BlockEnv (Map.insert ident (pos,(returnTyp,paramsTyps,nParams)) sigs) context  blockTyp)
    Just (pos',_) -> do
      putStrLn $ (show pos) ++ ": function "++ ident ++ " already declared in " ++ (show pos')
      return curr
      
--aggiunge parametri/argomenti all'environment
addParams::Env->[Argument]->IO Env
addParams env arguments = foldM addArg env arguments where
  addParam::Env->Argument->IO Env
  addParam (Env (current:stack)) (DefParam modal (Pident (pos,ident)) typ) = do
    newBlockEnv<-addVarDec current ident pos typ (Just modal)
    return (Env (newBlockEnv:stack))

--controlla gli statement
checkStm::Env->Stm->IO Env
checkStm env stm = case stm of
  Assgn _ lexp  expr -> do
    (pos,typ)<-inferExpr env lexp
    checkExpr env typ expr
    checkConstVar env lexp
    return env
  SExp expr -> do
    inferExpr env expr
    return env
  SimpleIf expr decs -> do
    pushEnv<-Ok $ pushNewBlocktoEnv env BTifEls
    checkExpr env Tbool expr
    checkDec pushEnv decs
    return env
  IfThElse expr decsIf decsElse -> do
    pushEnvIf<-Ok $ pushNewBlocktoEnv env BTifEls
    pushEnvElse<-Ok $ pushNewBlocktoEnv env BTifEls
    checkExpr env Tbool expr
    checkDec pushEnvIf decsIf
    checkDec pushEnvElse decsElse
    return env
  While decsIni expr decs -> do
    pushEnv<-Ok $ pushNewBlocktoEnv env BTloop
    initializationEnv<-checkDec pushEnv decsIni
    checkExpr initializationEnv Tbool expr
    checkDec initializationEnv decs
    return env
  DoWhile decs expr -> do
    checkExpr env Tbool expr
    pushEnv<-Ok $ pushNewBlocktoEnv env BTloop
    checkDec pushEnv decs
    return env
  Valreturn expr -> do
    (pos,typ)<-inferExpr env expr
    checkReturn env pos typ
    return env

--controlla l'expression  
checkExpr::Env->Typ->Exp->IO Env
checkExpr env typ expr= do
  (pos,exprTyp)<-inferExpr env expr
  genTyp<-generalize exprTyp typ
  if typ==genTyp
    then
      return env
    else
      fail $ (show pos) ++ ": Type mismatch.Expected type->" ++ (show typ) ++ ". Actual type->" ++ (show exprTyp)

--Generalizzazione dei tipi
generalize::Typ->Typ->IO Typ
generalize from to = Ok to --generalizzazione dal tipo from al tipo to
return to

genericType::Typ->Typ->IO Typ
genericType typ1 typ2 = do
  genTyp<-generalize typ2 typ1
  if genTyp==typ1
  then return genTyp
  else generalize typ1 typ2

inferExpr::Env->Exp->IO PosTyp
inferExpr env expr = case expr of

  InfixOp infixOp expr1 expr2 -> do
    posTyp<-inferInfixExpr env infixOp expr1 expr2
    return posTyp

  Unary_Op op  expr -> do
    posTyp<-inferUnaryExp env op expr
    return posTyp

  Addr expr -> do
    (pos,typ)<-inferExpr env expr
    return (pos,(TPointer typ))

  Indirection expr-> do
    (pos,typ)<-inferExpr env expr
    posTyp<-checkIfIsPointerAndReturnType pos typ
    return posTyp

  Arraysel exprArray exprInt -> do
    checkExpr env Tint exprInt 
    arrayPosTyp<-inferExpr env exprArray
    case arrayPosTyp of
      (pos,TArray typ) -> return (pos,typ)
      (pos,_)->fail $ (show pos) ++ ": " ++ "Cannot use array selection operand in non-array types"


inferUnaryExp::Env->Unary_Op->Exp->IO PosTyp
inferUnaryExp env op expr = case op of
  Neg -> do
    (pos,typ)<-inferExpr env expr
    checkIfIsNumeric pos typ
    return (pos,typ)
  Logneg -> do
    (pos,typ)<-inferExpr env expr
    checkIfBoolean pos typ
    return (pos,typ)

inferInfixExpr::Env->InfixOp->Exp->Exp->IO PosTyp
inferInfixExpr env infixOp expr1 expr2 = do
  e1@(pos1,typ1)<-inferExpr env expr1
  e2@(pos2,typ2)<-inferExpr env expr2
  genTyp<-genericType typ1 typ2
  gtyp1<-generalize typ1 genTyp
  gtyp2<-generalize typ2 genTyp
  case infixOp of
    ArithOp op -> do
      case op of
        Add->do
          checkIfIsNumeric pos1 gtyp1
          checkIfIsNumeric pos2 gtyp2
          return (pos1,genTyp)
        Sub->do
          checkIfIsNumeric pos1 gtyp1
          checkIfIsNumeric pos2 gtyp2
          return (pos1,genTyp)
        Mul->do
          checkIfIsNumeric pos1 gtyp1
          checkIfIsNumeric pos2 gtyp2
          return (pos1,genTyp)
        Div->do
          checkIfIsNumeric pos1 gtyp1
          checkIfIsNumeric pos2 gtyp2
          return (pos1,genTyp)
        Pow->do
          checkIfIsNumeric pos1 gtyp1
          checkIfIsNumeric pos2 gtyp2
          return (pos1,genTyp)
        Mod->do 
          checkIfIsNumeric pos1 gtyp1
          checkIfIsNumeric pos2 gtyp2
          return (pos1,genTyp)
    BoolOp op->do
      checkIfBoolean pos1 gtyp1
      checkIfBoolean pos2 gtyp2
      case op of
        And->do
          return (pos1,Tbool)
        Or->do
          return (pos1,Tbool)
    RelOp op->do
      checkExpr env genTyp expr2
      case op of
        Eq->do
          checkIfIsEq pos1 gtyp1
          checkIfIsEq pos2 gtyp2
          return (pos1,Tbool)
        Neq->do
          checkIfIsEq pos1 gtyp1
          checkIfIsEq pos2 gtyp2
          return (pos1,Tbool)
        Lt->do
          checkIfIsOrd pos1 gtyp1
          checkIfIsOrd pos2 gtyp2
          return (pos1,Tbool)
        LtE->do
          checkIfIsOrd pos1 gtyp1
          checkIfIsOrd pos2 gtyp2
          return (pos1,Tbool)
        Gt->do
          checkIfIsOrd pos1 gtyp1
          checkIfIsOrd pos2 gtyp2
          return (pos1,Tbool)
        GtE->do
          checkIfIsOrd pos1 gtyp1
          checkIfIsOrd pos2 gtyp2
          return (pos1,Tbool)  

checkIfIsNumeric::Pos->Typ->IO ()
checkIfIsNumeric pos typ = do
  if typ/=Tint && typ/=Tfloat
  then fail $ (show pos) ++ ": " ++ "Cannot use operand in non-numeric types"
  else return ()

checkIfBoolean::Pos->Typ->IO ()
checkIfBoolean pos typ = do
  if typ/=Tbool
  then fail $ (show pos) ++ ": " ++ "Cannot use operand in non-boolean types"
  else return ()  

---------------
----HELPERS----
---------------

getParamsTyp :: [Argument] -> [Typ]
getParamsTyp (par:params) = case par of
    Param typ _ -> typ:getParamsTyp params
    ParamArr typ _ _ -> (Tarray typ):getParamsTyp params

getParamsMod::[Parameter]->[Mod]
getParamsMod params = map (\(DefParam modal _ _ )->Just modal) params

createInitialEnv :: Env -> IO Env
createInitialEnv (Env (current:stack)) = do
  newBlockEnv <- addFuncDec current "writeInt" (-1,-1) Tvoid [Tint] [Just Modality_VAL] 1
  newBlockEnv <- addFuncDec newBlockEnv "writeFloat" (-1,-1) Tvoid [Tfloat] [Just Modality_VAL] 1
  newBlockEnv <- addFuncDec newBlockEnv "writeChar" (-1,-1) Tvoid [Tchar] [Just Modality_VAL] 1
  newBlockEnv <- addFuncDec newBlockEnv "writeString" (-1,-1) Tvoid [Tstring] [Just Modality_VAL] 1

  newBlockEnv <- addFuncDec newBlockEnv "readInt" (-1,-1)  Tint [] [] 0
  newBlockEnv <- addFuncDec newBlockEnv "readFloat" (-1,-1)  Tfloat [] [] 0
  newBlockEnv <- addFuncDec newBlockEnv "readChar" (-1,-1)  Tchar [] [] 0
  newBlockEnv <- addFuncDec newBlockEnv "readString" (-1,-1)  Tstring [] [] 0

  return (Env ((emptyBlockEnv BTdecs):newBlockEnv:stack))



--controlla se la Modality richiede una L-expr, se sì restituisco True, altrimenti False
modalityRequiresLexpr::Modality->Bool
modalityRequiresLexpr modal =
  if modal==Modality_RES || modal==Modality_VALRES || modal==Modality_REF
    then True
    else False
  
--controlla se si tratta di una L-expr restituendo un Bool
isLexpr::Exp->Bool
isLexpr expr = case expr of
  Evar _ -> True
  Indirection _ -> True
  Arraysel _ _ -> True
  otherwise -> False

getVarFromArraySelection :: Exp -> IO Pident
getVarFromArraySelection expr = case expr of
  Evar pident -> do
    return pident
  Arraysel exprArray _ -> getVarFromArraySelection exprArray
