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

--Generalizzazione dei tipi
generalize::Typ->Typ->Err Typ
generalize from to = Ok to --generalizzazione dal tipo from al tipo to

--Controllo se la Modality richiede una L-expr, se sì restituisco True, altrimenti False
modalityRequiresLexpr::Modality->Bool
modalityRequiresLexpr modal =
  if modal==Modality_RES || modal==Modality_VALRES || modal==Modality_REF
    then True
    else False
  
--Controllo se si tratta di una L-expr restituendo un Bool
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
