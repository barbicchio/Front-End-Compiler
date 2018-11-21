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
  deriving (Eq, Ord, Show, Read)--elenco typeclass

data BlockEnv = BlockEnv {
  funDefs :: Sigs, --funDefs ha tipo Sigs
  varDefs :: Context, --varDefs ha tipo Context
  blockTyp :: BlockTyp --blockTyp ha tipo BlockTyp
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


checkDecStm::Env->DecStm->IO Env
checkDecStm env decStm = do
					newEnv <- foldM addDec env fundecs
					case decStm of
  						Dec dec -> do
  						newEnv<-foldM checkDec newEnv decsstats
    					return newEnv
  						Stmt stm -> do
    					checkStm env stm
    					return env

checkDec::Env->Dec->IO Env
checkDec env dec = do
  newEnv<- addDec env dec
  case dec of
    VarDeclar typ pident expr -> case expr of
      Nothing -> do 
      	newEnv <- addDec env dec
      	return newEnv
      Just expr->do 
      	newEnv <- addDec env dec
        checkExpr newEnv typ expr
        return newEnv
    
    Func retTyp pident@(Pident (pos,ident)) params _  decs stms-> do
      newblock<-(pushNewBlocktoEnv newEnv (BTfun retTyp))
      pushEnv<-addParams newblock params-- env con il nuovo layer e i parametri della funzione inseriti
      pushEnv<-checkDecs pushEnv decs
      checkStms pushEnv stms
   --se ha un tipo di ritorno controlla l'esistenza di un return (il controllo di tipo viene fatto dopo)
      if retTyp/=Tvoid
      then do
        ret<-findReturnInStms stms
        case ret of
          False -> do 
                putStrLn $ (show pos) ++ ": Missing return statement for function" ++ (show ident)
                return env 
          True -> return newEnv
      --se non ha un tipo di ritorno anche l'assenza di un return è accettata
      else return newEnv

findReturnInStms::[Stm]->IO Bool
findReturnInStms stms = do
  returns<-mapM findReturnInStm stms
  return $ or returns  

findReturnInStm::Stm->IO Bool
findReturnInStm stm = case stm of
    Valreturn _ -> return True
    SimpleIf _ stms -> findReturnInStms stms
    IfThElse _ stmsIf stmsElse -> do
    	returnIf <- findReturnInStms stmsIf
    	returnElse <- findReturnInStms stmsElse
    	return (returnIf || returnElse)
    While _ _ stms -> findReturnInStms stms
    DoWhile stms _ -> findReturnInStms stms
    _ -> return False



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
    checkDecs pushEnv decs
    return env
  IfThElse expr decsIf decsElse -> do
    pushEnvIf<-Ok $ pushNewBlocktoEnv env BTifEls
    pushEnvElse<-Ok $ pushNewBlocktoEnv env BTifEls
    checkExpr env Tbool expr
    checkDecs pushEnvIf decsIf
    checkDecs pushEnvElse decsElse
    return env
  While decsIni expr decs -> do
    pushEnv<-Ok $ pushNewBlocktoEnv env BTloop
    initializationEnv<-checkDecs pushEnv decsIni
    checkExpr initializationEnv Tbool expr
    checkDecs initializationEnv decsStms
    return env
  DoWhile decsStms expr -> do
    checkExpr env Tbool expr
    pushEnv<-Ok $ pushNewBlocktoEnv env BTloop
    checkDecs pushEnv decsStms
    return env
  Valreturn expr -> do
    (pos,typ)<-inferExpr env expr
    checkReturn env pos typ
    return env


--controlla che il valore tornato dal return sia compatibile con quello della dichiarazione
checkReturn::Env->Pos->Typ->IO ()
checkReturn (Env ((BlockEnv _ _ blockTyp):stack)) pos returnTyp= case blockTyp of
  BTfun decTyp -> do
    genTyp<-generalize returnTyp decTyp
    if genTyp/=decTyp
    then fail $ (show pos) ++ ": Type mismatch in return statement.Expected type->" ++ (show decTyp) ++ ". Actual type->" ++ (show returnTyp)
    else return ()
  otherwise->checkReturn (Env stack) pos returnTyp


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


checkDefaultStm::Env->Maybe Stm->IO ()
checkDefaultStm env defaultStm = case defaultStm of
  Just stm-> do
    checkStm env stm
    return ()
  Nothing -> do return ()

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

  Ls (expr:exprs)-> do
    (pos,typ)<-inferExpr env expr
    checkLs env pos typ exprs
    return (pos,TArray typ)

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


  Fcall pident@(Pident (pos,ident)) callExprs callNParams ->do
    posTypLs <- mapM (inferExpr env) callExprs --trova la lista di PosTyp
    callParams <- mapM (\(pos,typ) -> do return typ) posTypLs --Ritorna la lista di Typ dal PosTyp
    (_,(defParams,retTyp,defNParams))<-lookFunc pident env --trova il tipo di ritorno ( se la funzione esiste)
    paramsTyps<- mapM (\(typ,modal)->do return typ) defParams --trova la lista del tipo di parametri della definizione a partire da [ModTyp]
    checkParams pos callParams callNParams paramsTyps defNParams --check dei tipi sui parametri passati
    checkModality env pident callExprs -- controllo sulla modalità dei parametri attuali rispetto alla definizione di funzione
    return (pos,retTyp)

  Efloat (Pfloat (pos,val)) -> do
    return (pos,Tfloat)
  Eint (Pint (pos,val)) -> do
    return (pos,Tint)
  Ebool (Pbool (pos,val)) -> do
    return (pos,Tbool)
  Estring (Pstring (pos,val)) -> do
    return (pos,Tstring)
  Echar (Pchar (pos,val)) -> do
    return (pos,Tchar)
  Evar pident@(Pident (pos,ident)) -> do
    (_,(typ,modal))<-lookVar pident env
    return (pos,typ)

checkLs::Env->Pos->Typ->[Exp]->IO Env
checkLs env pos typ exprs = case exprs of
  []->return env
  (expr:expss)->case (checkExpr env typ expr) of
    Ok _ -> do
      checkLs env pos typ expss
      return env
    Bad _ -> fail $ (show pos) ++ ": Multiple different types in the same array definition"

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



checkIfIsEq::Pos->Typ->IO ()
checkIfIsEq pos typ = case typ of
  TArray _ -> do fail $ (show pos) ++ ": " ++ "Cannot use operand in non-comparable types"
  otherwise -> do return ()

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

checkIfIsOrd::Pos->Typ->IO ()
checkIfIsOrd pos typ = do
  if typ/=Tint && typ/=Tfloat
  then fail $ (show pos) ++ ": " ++ "Cannot use operand in non-ordered types"
  else return ()

--controlla se il typo passato è un pointer, nel caso lo sia torna il tipo di array,
--altrimenti fail
checkIfIsPointerAndReturnType::Pos->Typ->IO PosTyp
checkIfIsPointerAndReturnType pos typ = case typ of
  Tpointer ptyp -> return (pos,ptyp)
  _ -> fail $ (show pos) ++ ": " ++ "Cannot use operand in non-pointer types"

--controlla numero e tipo dei parametri di una chiamata a funzione
checkParams::Pos->[Typ]->Int->[Typ]->Int->IO ()
checkParams pos callParams callNParams defParams defNParams = do
  if callNParams /= defNParams
  then fail $ (show pos) ++ ": " ++ (show defNParams) ++ " parameters expected, but " ++ (show callNParams) ++ " found"
  else do
    checkParamsTyps pos (zip (zip callParams defParams) [1,2..])
    return ()

checkParamsTyps::Pos->[((Typ,Typ),Int)]->IO ()
checkParamsTyps pos list = case list of
  [] -> return ()
  (((typCall,typDef),paramN):params) -> do
    genTyp<-generalize typCall typDef
    if genTyp/=typDef
    then fail $ (show pos) ++ ": Type mismatch in parameter "++ (show paramN) ++".Expected type->" ++ (show typDef) ++ ". Actual type->" ++ (show typCall)
    else do
      checkParamsTyps pos params

---------------------
-----ENV LOOKUP------
---------------------

lookVar::Pident->Env->IO PosTypMod
lookVar pident@(Pident (pos,ident)) (Env stack) = case stack of
  [] -> fail $ (show pos) ++ ": variable " ++ (show ident) ++ " out of scope"
  (current@(BlockEnv _ context _ ):parent) -> do
    maybePosTypMod <- lookVarInContext ident context
    case maybePosTypMod of
      Nothing -> lookVar pident (Env parent)
      Just posTypMod-> return posTypMod

lookFunc::Pident->Env->IO PosSig
lookFunc pident@(Pident (pos,ident)) (Env stack) = case stack of
  [] -> fail $ (show pos) ++ ": function " ++ (show ident) ++ " out of scope"
  (current@(BlockEnv sigs _ _ ):parent) -> do
    maybePosTyp <- lookFuncInSigs ident sigs
    case maybePosTyp of
      Nothing -> lookFunc pident (Env parent)
      Just posTyp-> return posTyp


lookVarInContext::Ident->Context->IO (Maybe PosTypMod)
lookVarInContext ident context= do
  return (Map.lookup ident context)

lookFuncInSigs::Ident->Sigs->IO (Maybe PosSig)
lookFuncInSigs ident sigs= do
  return (Map.lookup ident sigs)    

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
  VarDeclar typ pident@(Pident (pos,ident)) _ -> do
    newBlockEnv <- addVarDec current ident pos typ
    return (Env (newBlockEnv:stack))
 Func typ pident@(Pident (pos,ident)) params nParams _ _ -> do
    newBlockEnv <- addFuncDec current ident pos typ (getParamsTyp params) nParams
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
addParams env parameters = foldM addParam env parameters
 
addParam::Env->Argument->IO Env
addParam (Env (current:stack)) pars = case pars of
    Param typ (Pident (pos,ident)) -> do
      newBlockEnv<-addVarDec current ident pos typ
      return (Env (newBlockEnv:stack))
    ParamArr typ (Pident (pos,ident)) _ -> do
      newBlockEnv<-addVarDec current ident pos (Tarray typ)
      return (Env (newBlockEnv:stack))

---------------
----HELPERS----
---------------

getParamsTyp :: [Argument] -> [Typ]
getParamsTyp (par:params) = case par of
    Param typ _ -> typ:getParamsTyp params
    ParamArr typ _ _ -> (Tarray typ):getParamsTyp params


getParamsTyp::[Argument]->[Typ]
getParamsTyp params = map (\(Param typ _)->typ) params

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


------------------------
-----MODALITY CHECK-----
------------------------
--controlla se la modalità della definizione e i parametri attuali della chiamata sono compatibili
checkModality::Env->Pident->[Exp]->IO ()
checkModality env pident callExprs = do

  --trova la posizione dei parametri
  posTypLs <- mapM (inferExpr env) callExprs --trova la lista di PosTyp
  posLs <- mapM (\(pos,typ) -> do return pos) posTypLs --Ritorna la lista di Pos dal PosTyp
  --trova le modalità dei parametri formali
  (pos,(typModLs,_,_))<-lookFunc pident env
  --struttura dati supporto
  triples<- Ok (zip3 posLs callExprs typModLs)
  --controlla che le espressioni siano lexpr in base alla modalità dei parametri
  mapM checkLexpr triples

  --controlla che non vengano passate costanti in una modalità che assegna un valore
  mapM (checkConstCall env) triples

  return ()

--controlla se il parametro attuale è una Lexpr se la modalità lo richiede
checkLexpr::(Pos,Exp,TypMod)->IO ()
checkLexpr (pos,expr,(typ,Just modal)) = do
  if  modalityRequiresLexpr modal
  then
    if isLexpr expr
    then return ()
    else fail $ (show pos) ++ ":" ++ "Parameter Modality requires an L-Expression"
  else return ()

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

checkConstCall::Env->(Pos,Exp,TypMod)->IO ()
checkConstCall env (_,expr,(typ,Just modal)) = do
  case expr of
    Evar (pident@(Pident (pos,ident)))->do
      var<-lookVar pident env
      case var of
        (_,(_,(Just varmodal)))->do
          if (varmodal==Modality_CONST) && modalityRequiresLexpr modal
            then
              fail $ (show pos) ++ ":" ++ "Cannot pass parameter by constant when Modality requires an L-Expression"
            else
              return ()
        otherwise->return ()
    Arraysel exprArray _ -> do
      (pident@(Pident (pos,ident)))<- getVarFromArraySelection exprArray
      var<-lookVar pident env
      case var of
        (_,(_,varmod@(Just varmodal)))->do
          if varmodal==Modality_CONST && modalityRequiresLexpr modal
            then  fail $ (show pos) ++ ":" ++ "Cannot pass an index of constant array when Modality requires an L-Expression"
            else return ()
        otherwise->return ()
    otherwise->return ()

checkConstVar::Env->Exp->IO ()
checkConstVar env expr = do
  case expr of
    Evar (pident@(Pident (pos,ident)))->do
      var<-lookVar pident env
      case var of
        (_,(_,varmod@(Just varmodal)))->do
          if varmodal==Modality_CONST
            then  fail $ (show pos) ++ ":" ++ "Cannot assign a value to a CONST variable"
            else return ()
        otherwise->return ()
    Arraysel exprArray _ -> do
      (pident@(Pident (pos,ident)))<- getVarFromArraySelection exprArray
      var<-lookVar pident env
      case var of
        (_,(_,varmod@(Just varmodal)))->do
          if varmodal==Modality_CONST
            then  fail $ (show pos) ++ ":" ++ "Cannot assign a value to an index of a CONST array"
            else return ()
        otherwise->return ()
    otherwise->return ()


getVarFromArraySelection :: Exp -> IO Pident
getVarFromArraySelection expr = case expr of
  Evar pident -> do
    return pident
  Arraysel exprArray _ -> getVarFromArraySelection exprArray
