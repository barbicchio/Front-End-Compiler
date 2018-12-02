module TypeCheckerLua where

import System.IO
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

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
type Context = Map.Map Ident PosTypMod

type Sig = ([TypMod],Typ,Int)
type Pos = (Int,Int)
type PosTyp = (Pos,Typ)
type PosSig = (Pos,Sig)
type TypMod = (Typ,Mod)
type PosTypMod = (Pos,TypMod)

---------------------
-----TYPECHECKER-----
---------------------

test::Program->[String]
test program = execWriter (typecheck program)

typecheck :: Program -> Writer [String] ()
typecheck p@(Progr decls) = do
  env <- createInitialEnv emptyEnv
  checkDecs env decls
  return ()

gettypid::Exp->String  
gettypid exp=case exp of
  Eint (Pint(_,id))->id
  Ebool (Pbool(_,id))->id
  Estring (Pstring(_,id))->id
  Efloat (Preal(_,id))->id
  Echar (Pchar(_,id))->id
  Evar (Pident(_,id))->id
  otherwise-> show(exp)

checkDecs :: Env -> [Dec] -> Writer [String] Env
checkDecs env decsstats = do 
newEnv <- foldM addDec env fundecs
foldM checkDec newEnv decsstats --foldM analogo di foldl per le monadi
  where fundecs = filterdecs decsstats

filterdecs :: [Dec] -> [Dec]
filterdecs [] = []
filterdecs (dec:decs) = case dec of
  Func _ _ _ _ _  -> dec: filterdecs decs
  otherwise -> filterdecs decs

checkDecStms::Env->[DecStm]->Writer [String] Env
checkDecStms env decsstms = do
  newEnv <- foldM addDec env fundecs
  foldM checkDecStm newEnv decsstms 
   where fundecs = filterdecstmts decsstms  

filterdecstmts :: [DecStm] -> [Dec]
filterdecstmts  [] = []
filterdecstmts  (decstm:decstmts) = case decstm of
  Dec dec@(Func _ _ _ _ _)  -> dec: (filterdecstmts  decstmts)
  otherwise -> filterdecstmts decstmts


checkDecStm::Env->DecStm->Writer [String] Env
checkDecStm env decStm = case decStm of
             Dec dec -> do
              newEnv<-checkDec env dec
              return newEnv --controllare che ritorni l'ambiente corretto
             Stmt stm -> do
              checkStm env stm
              return env

checkDec::Env->Dec->Writer [String] Env
checkDec env dec = case dec of
    VarDeclar typ pident expr -> case expr of
      Nothing -> do 
        newEnv <- addDec env dec
        return newEnv
      Just expr->do 
        newEnv <- addDec env dec
        checkExpr newEnv typ expr
        return newEnv  
    Func retTyp pident@(Pident (pos,ident)) params _  decStm-> do
      newblock<-(pushNewBlocktoEnv env (BTfun retTyp))
      pushEnv<-addParams newblock params-- env con il nuovo layer e i parametri della funzione inseriti
      checkDecStms pushEnv decStm --{--pushEnv<--}non serve che torni pushEnv
   --se ha un tipo di ritorno controlla l'esistenza di un return (il controllo di tipo viene fatto dopo)
      if retTyp/=Tvoid
      then do
        ret<-findReturnInDecStms decStm
        case ret of
          False -> do 
                tell $[(show pos) ++ ": Missing return statement for function "++ ident]
                return env 
          True -> return env
      --se non ha un tipo di ritorno anche l'assenza di un return è accettata
      else return env

findReturnInDecStms::[DecStm]->Writer [String] Bool
findReturnInDecStms decStm = do
  returns<-mapM findReturnInStm decStm
  return $ or returns


findReturnInStm::DecStm->Writer [String] Bool
findReturnInStm decstm = case decstm of
    Stmt(Valreturn _ )-> return True
    Stmt(SimpleIf _ decstmts) -> findReturnInDecStms decstmts
    Stmt(IfThElse _ decstmsIf decstmsElse) -> do
      returnIf <- findReturnInDecStms decstmsIf
      returnElse <- findReturnInDecStms decstmsElse
      return (returnIf || returnElse)
    Stmt(While _  decstmts) -> findReturnInDecStms decstmts
    Stmt(DoWhile decstmts _)-> findReturnInDecStms decstmts
    _ -> return False

--controlla gli statement
checkStm::Env->Stm->Writer [String] Env
checkStm env stm = case stm of
  Assgn _ lexp expr -> do
    (pos,typ)<-inferExpr env lexp
    if typ==Terror
     then return env
     else do
     checkExpr env typ expr
     checkConstVar env lexp
     return env
  SExp expr -> do
    inferExpr env expr
    return env
  SimpleIf expr decstms -> do
    pushEnv<- pushNewBlocktoEnv env BTifEls
    checkExpr env Tbool expr
    checkDecStms pushEnv decstms
    return env
  IfThElse expr decsIf decsElse -> do
    pushEnvIf<- pushNewBlocktoEnv env BTifEls
    pushEnvElse<- pushNewBlocktoEnv env BTifEls
    checkExpr env Tbool expr
    checkDecStms pushEnvIf decsIf
    checkDecStms pushEnvElse decsElse
    return env
  While expr decsStms -> do
    pushEnv<-pushNewBlocktoEnv env BTloop
    checkExpr pushEnv Tbool expr
    checkDecStms pushEnv decsStms
    return env
  DoWhile decsStms expr -> do
    checkExpr env Tbool expr
    pushEnv<-pushNewBlocktoEnv env BTloop
    checkDecStms pushEnv decsStms
    return env
  Valreturn expr -> do
    (pos,typ)<-inferExpr env expr
    if typ==Terror
    then return env
    else do
    checkReturn env pos typ expr
    return env


--controlla che il valore tornato dal return sia compatibile con quello della dichiarazione
checkReturn::Env->Pos->Typ->Exp->Writer [String] ()
checkReturn (Env ((BlockEnv _ _ blockTyp):stack)) pos returnTyp exp= case blockTyp of
  BTfun decTyp -> do
    genTyp<-generalize returnTyp decTyp
    if genTyp/=decTyp
    then do
      let ident=gettypid exp
      tell $ [(show pos) ++ ": Type mismatch in return statement. Expected type->" ++ (show decTyp) ++ ". Actual type->" ++ (show returnTyp)++" in token "++ident]
    else return ()
  otherwise->checkReturn (Env stack) pos returnTyp exp

checksubtyp::Pos->Typ->Typ->Writer[String]()
checksubtyp pos typ1 typ2=case (typ1,typ2) of
 (Tarray _ subtyp1,Tarray _ subtyp2)->checksubtyp pos subtyp1 subtyp2
 otherwise->if (typ1==typ2)
            then return ()
            else do
            tell $ [(show pos) ++ ": Type mismatch in array. Expected type->" ++ (show typ1) ++ ". Actual type->" ++ (show typ2)]
            return()



--controlla l'expression. 
checkExpr::Env->Typ->Exp->Writer [String] (Env)
checkExpr env typ expr= do
  (pos,exprTyp)<-inferExpr env expr
  case (typ,exprTyp) of
    (Tarray _ subtyp,Tarray _ subexprTyp)-> do
      checksubtyp pos subtyp subexprTyp
      return env
    (Tpointer subtyp,Tpointer subexprTyp)->do
      checksubtyp pos subtyp subexprTyp
      return env
    otherwise->do
     if exprTyp==Terror
      then return env
      else do  
      genTyp<-generalize exprTyp typ
      if typ==genTyp
       then
       return env
       else do
       let ident=gettypid expr
       tell $[(show pos) ++ ": Type mismatch. Expected type->" ++ (show typ) ++ ". Actual type token "++ident++ "-> " ++ (show exprTyp)]
       return env


checkDefaultStm::Env->Maybe Stm->Writer [String] ()
checkDefaultStm env defaultStm = case defaultStm of
  Just stm-> do
    checkStm env stm
    return ()
  Nothing -> do return ()

--Generalizzazione dei tipi
generalize::Typ->Typ->Writer [String] Typ
generalize Tint Tfloat = do
 return Tfloat
--generalize (Tarray Nothing Tint) (Tarray Nothing Tfloat) = do --generalizzazione e cast per array
 --return (Tarray Nothing Tfloat)
generalize from to = do
 return from --nel caso non si possa generalizzare


genericType::Typ->Typ->Writer [String] Typ
genericType typ1 typ2 = do
  genTyp<-generalize typ2 typ1
  if genTyp==typ1
  then return genTyp
  else generalize typ1 typ2

inferExpr::Env->Exp->Writer [String] PosTyp
inferExpr env expr = case expr of

  Arr list@(exp:exprs)-> do
    (pos,typ)<-inferExpr env exp
    checkArr env pos typ exprs
    return (pos,Tarray Nothing typ) 

  InfixOp infixOp expr1 expr2 -> do
    posTyp<-inferInfixExpr env infixOp expr1 expr2
    return posTyp

  Unary_Op op  exp -> do
    posTyp<-inferUnaryExp env op exp
    return posTyp

  Addr exp -> do
    (pos,typ)<-inferExpr env exp
    return (pos,(Tpointer typ))

  Indirection exp-> do
    (pos,typ)<-inferExpr env exp
    posTyp<-checkIfIsPointerAndReturnType pos typ exp
    return posTyp

  Arraysel exprArray exprInt -> do --investigare su exprArray 
    checkExpr env Tint exprInt 
    arrayPosTyp<-inferExpr env exprArray
    case arrayPosTyp of
      (pos,Terror)->return ((-1,-1),Terror)
      (pos,Tarray _ typ) -> return (pos,typ)
      (pos,_)->do
        let ident=gettypid expr
        tell $ [(show pos) ++": " ++ "Cannot use array selection operand in non-array type "++ ident]
        return((-1,-1),Terror)
  PrePost _ exp->do
    (pos,typ)<-inferExpr env exp
    posTyp<-checkIfIsInt pos typ exp
    return (pos,typ)
  
  Fcall pident@(Pident (pos,ident)) callExprs callNParams ->do
    posTypLs <- mapM (inferExpr env) callExprs --trova la lista di PosTyp
    callParams <- mapM (\(pos,typ) -> do return typ) posTypLs --Ritorna la lista di Typ dal PosTyp
    (_,(defParams,retTyp,defNParams))<-lookFunc pident env --trova il tipo di ritorno ( se la funzione esiste)
    if retTyp /= Terror --continuo con il controllo dei parametri della funzione solo se il tipo di ritorno è diverso da Terror
    then do 
      paramsTyps<- mapM (\(typ,modal)->do return typ) defParams --trova la lista del tipo di parametri della definizione a partire da [ModTyp]
      checkParams pos callParams callNParams paramsTyps defNParams ident --check dei tipi sui parametri passati
      checkModality env pident callExprs -- controllo sulla modalità dei parametri attuali rispetto alla definizione di funzione
      return (pos,retTyp)
    else return (pos,retTyp)
  
  Efloat (Preal (pos,val)) -> do
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

checkArr::Env->Pos->Typ->[Exp]->Writer [String] ()
checkArr env pos typ exprs = case exprs of
  []->return ()
  (expr:expss)-> do
    checkExpr env typ expr
    checkArr env pos typ expss

inferUnaryExp::Env->Unary_Op->Exp->Writer [String] PosTyp
inferUnaryExp env op expr = case op of
  Neg -> do
    (pos,typ)<-inferExpr env expr
    checkIfIsNumeric pos typ expr
    return (pos,typ)
  Logneg -> do
    (pos,typ)<-inferExpr env expr
    checkIfBoolean pos typ expr 
    return (pos,typ)

inferInfixExpr::Env->InfixOp->Exp->Exp->Writer [String] PosTyp
inferInfixExpr env infixOp expr1 expr2 = do
  e1@(pos1,typ1)<-inferExpr env expr1
  e2@(pos2,typ2)<-inferExpr env expr2
  if typ1==Terror || typ2==Terror
   then return (pos1,Terror)
  else do     
  genTyp<-genericType typ1 typ2
  gtyp1<-generalize typ1 genTyp
  gtyp2<-generalize typ2 genTyp
  case infixOp of
    ArithOp op -> do
      case op of
        Add->do
          checkIfIsNumeric pos1 gtyp1 expr1
          checkIfIsNumeric pos2 gtyp2 expr2
          return (pos1,genTyp)
        Sub->do
          checkIfIsNumeric pos1 gtyp1 expr1
          checkIfIsNumeric pos2 gtyp2 expr2
          return (pos1,genTyp)
        Mul->do
          checkIfIsNumeric pos1 gtyp1 expr1
          checkIfIsNumeric pos2 gtyp2 expr2
          return (pos1,genTyp)
        Div->do
          checkIfIsNumeric pos1 gtyp1 expr1
          checkIfIsNumeric pos2 gtyp2 expr2
          return (pos1,genTyp)
        Pow->do
          checkIfIsNumeric pos1 gtyp1 expr1
          checkIfIsNumeric pos2 gtyp2 expr2
          return (pos1,genTyp)
        Mod->do 
          checkIfIsNumeric pos1 gtyp1 expr1
          checkIfIsNumeric pos2 gtyp2 expr2
          return (pos1,genTyp)
    BoolOp op->do
      checkIfBoolean pos1 gtyp1 expr1
      checkIfBoolean pos2 gtyp2 expr2
      case op of
        And->do
          return (pos1,Tbool)
        Or->do
          return (pos1,Tbool)
    RelOp op->do
      checkExpr env genTyp expr2
      case op of
        Eq->do
          checkIfIsEq pos1 gtyp1 expr1
          checkIfIsEq pos2 gtyp2 expr2
          return (pos1,Tbool)
        Neq->do
          checkIfIsEq pos1 gtyp1 expr1
          checkIfIsEq pos2 gtyp2 expr2
          return (pos1,Tbool)
        Lt->do
          checkIfIsOrd pos1 gtyp1 expr1
          checkIfIsOrd pos2 gtyp2 expr2
          return (pos1,Tbool)
        LtE->do
          checkIfIsOrd pos1 gtyp1 expr1
          checkIfIsOrd pos2 gtyp2 expr2
          return (pos1,Tbool)
        Gt->do
          checkIfIsOrd pos1 gtyp1 expr1
          checkIfIsOrd pos2 gtyp2 expr2
          return (pos1,Tbool)
        GtE->do
          checkIfIsOrd pos1 gtyp1 expr1
          checkIfIsOrd pos2 gtyp2 expr2
          return (pos1,Tbool)  

checkIfIsEq::Pos->Typ->Exp->Writer [String] ()
checkIfIsEq pos typ exp = case typ of
  Tarray _ _ -> do 
    let ident=gettypid exp
    tell $ [(show pos) ++": " ++ "Cannot use operand in non-comparable type "++ident]
  otherwise -> do return ()

checkIfIsInt::Pos->Typ->Exp->Writer [String] ()
checkIfIsInt pos typ exp = do
  if typ/=Tint 
  then do
    let ident=gettypid exp
    tell $ [(show pos) ++ ": " ++ "Cannot use operand in non-int type "++ident]
  else return ()

checkIfIsNumeric::Pos->Typ->Exp->Writer [String] ()
checkIfIsNumeric pos typ exp = do
  if typ/=Tint && typ/=Tfloat
  then do
    let ident=gettypid exp
    tell $ [(show pos) ++ ": " ++ "Cannot use operand in non-numeric type "++ident]
  else return ()

checkIfBoolean::Pos->Typ->Exp->Writer [String] ()
checkIfBoolean pos typ exp = do
  if typ/=Tbool
  then do
    let ident=gettypid exp
    tell $ [(show pos) ++":"++"Cannot use operand in non-boolean type "++ident]
  else return ()  

checkIfIsOrd::Pos->Typ->Exp->Writer [String] ()
checkIfIsOrd pos typ exp = do
  if typ/=Tint && typ/=Tfloat
  then do
    let ident=gettypid exp 
    tell $ [(show pos) ++ ": "++ "Cannot use operand in non-ordered type "++ident]
  else return ()

--controlla se il typo passato è un pointer, nel caso lo sia torna il tipo "base" del pointer,
--altrimenti fail
checkIfIsPointerAndReturnType::Pos->Typ->Exp->Writer [String] PosTyp
checkIfIsPointerAndReturnType pos typ exp = case typ of
  Tpointer ptyp -> return (pos,ptyp)
  _ -> do
    let ident=gettypid exp
    tell $ [(show pos) ++":"++"Cannot use operand in non-pointer type "++ident]
    return ((-1,-1),Terror)

--controlla numero e tipo dei parametri di una chiamata a funzione
checkParams::Pos->[Typ]->Int->[Typ]->Int->String->Writer [String] ()
checkParams pos callParams callNParams defParams defNParams ident= do
  if callNParams /= defNParams
  then tell $ [(show pos)++ ": "++"function "++ident++"expects"++ (show defNParams) ++ "parameters but " ++ (show callNParams) ++ " found"]
  else do
    checkParamsTyps pos (zip (zip callParams defParams) [1,2..]) ident
    return ()

checkParamsTyps::Pos->[((Typ,Typ),Int)]->String->Writer [String] ()
checkParamsTyps pos list ident = case list of
  [] -> return ()
  (((typCall,typDef),paramN):params) -> do
    genTyp<-generalize typCall typDef
    if genTyp/=typDef
    then tell $ [(show pos) ++ ": Type mismatch in parameter "++ (show paramN) ++".Expected type->" ++ (show typDef) ++ ". Actual type of token "++ident++"->" ++ (show typCall)]
    else do
      checkParamsTyps pos params ident

---------------------
-----ENV LOOKUP------
---------------------

lookVar::Pident->Env->Writer [String] PosTypMod
lookVar pident@(Pident (pos,ident)) (Env stack) = case stack of
  [] -> do
   tell $[(show pos) ++ ": variable " ++ ident ++ " out of scope"]
   return((-1,-1),(Terror,Nothing))
  (current@(BlockEnv _ context _ ):parent) -> do
    maybePosTypMod <- lookVarInContext ident context
    case maybePosTypMod of
      Nothing -> lookVar pident (Env parent)
      Just posTypMod-> return posTypMod

lookFunc::Pident->Env->Writer [String] PosSig
lookFunc pident@(Pident (pos,ident)) (Env stack) = case stack of
  [] -> do
    tell $[(show pos) ++ ": function " ++ ident ++ " out of scope"]
    return ((-1,-1),([],Terror,0))
  (current@(BlockEnv sigs _ _ ):parent) -> do
    maybePosTyp <- lookFuncInSigs ident sigs
    case maybePosTyp of
      Nothing -> lookFunc pident (Env parent)
      Just posTyp-> return posTyp


lookVarInContext::Ident->Context->Writer [String] (Maybe PosTypMod)
lookVarInContext ident context= do
  return (Map.lookup ident context)

lookFuncInSigs::Ident->Sigs->Writer [String] (Maybe PosSig)
lookFuncInSigs ident sigs= do
  return (Map.lookup ident sigs)    

---------------------
----ENV FUNCTIONS----
---------------------

emptyEnv = Env [emptyBlockEnv BTroot]
emptyBlockEnv blockTyp = BlockEnv Map.empty Map.empty blockTyp
newBlockEnv :: BlockTyp -> BlockEnv
newBlockEnv blockTyp = BlockEnv Map.empty Map.empty blockTyp

pushNewBlocktoEnv :: Env -> BlockTyp -> Writer [String] Env
pushNewBlocktoEnv (Env blocks) blocktyp = return $ Env ((newBlockEnv blocktyp):blocks)

--smista i tipi di dichiarazione da aggiungere all'env e aggiunge nel contesto corrente
addDec :: Env -> Dec -> Writer [String] Env
addDec env@(Env (current:stack)) dec = case dec of
  VarDeclar typ pident@(Pident (pos,ident)) _ -> do
    newBlockEnv <- addVarDec current ident pos (typ, Nothing)
    return (Env (newBlockEnv:stack))
  Func typ pident@(Pident (pos,ident)) params nParams _  -> do
    newBlockEnv <- addFuncDec current ident pos typ (getParamsModTyp params) nParams
    return (Env (newBlockEnv:stack))
  
--aggiunge una variabile a un contesto
addVarDec :: BlockEnv -> Ident -> Pos -> (Typ,Maybe Modality) -> Writer [String] BlockEnv
addVarDec curr@(BlockEnv sigs context blockTyp) ident pos@(line,col) modtyp = do
  record <- lookVarInContext ident context
  case record of
    Nothing -> return (BlockEnv sigs (Map.insert ident (pos,modtyp) context ) blockTyp) 
    Just (pos',_) -> do
      tell $[(show pos) ++ ": variable "++ ident ++ " already declared in " ++ (show pos')]
      return curr

--aggiunge una funzione a un contesto
addFuncDec :: BlockEnv -> Ident -> Pos -> Typ -> [(Typ, Mod)] -> Int -> Writer [String] BlockEnv
addFuncDec curr@(BlockEnv sigs context blockTyp) ident pos@(line,col) returnTyp paramsTyps nParams = do
  record <- lookFuncInSigs ident sigs
  case record of
    Nothing -> return (BlockEnv (Map.insert ident (pos,(paramsTyps,returnTyp,nParams)) sigs) context  blockTyp)
    Just (pos',_) -> do
      tell $ [(show pos) ++ ": function "++ ident ++ " already declared in " ++ (show pos')]
      return curr
      
addParams::Env->[Argument]->Writer [String] Env
addParams env arguments = foldM addParam env arguments where
  addParam::Env->Argument->Writer [String] Env
  addParam (Env (current:stack)) (FormPar modal typ (Pident (pos,ident))) = do
    newBlockEnv<-addVarDec current ident pos (typ,Just modal)
    return (Env (newBlockEnv:stack))      

---------------
----HELPERS----
---------------

getParamsModTyp::[Argument]->[(Typ,Mod)]
getParamsModTyp params = map (\(FormPar  mod typ _)->(typ,(Just mod))) params

createInitialEnv :: Env -> Writer [String] Env
createInitialEnv (Env (current:stack)) = do
  newBlockEnv <- addFuncDec current "writeInt" (-1,-1) Tvoid [(Tint,Just Modality_VAL)]  1
  newBlockEnv <- addFuncDec newBlockEnv "writeFloat" (-1,-1) Tvoid [(Tfloat,Just Modality_VAL)] 1
  newBlockEnv <- addFuncDec newBlockEnv "writeChar" (-1,-1) Tvoid [(Tchar,Just Modality_VAL)] 1
  newBlockEnv <- addFuncDec newBlockEnv "writeString" (-1,-1) Tvoid [(Tstring,Just Modality_VAL)] 1

  newBlockEnv <- addFuncDec newBlockEnv "readInt" (-1,-1)  Tint [] 0  
  newBlockEnv <- addFuncDec newBlockEnv "readFloat" (-1,-1)  Tfloat [] 0 
  newBlockEnv <- addFuncDec newBlockEnv "readChar" (-1,-1)  Tchar [] 0
  newBlockEnv <- addFuncDec newBlockEnv "readString" (-1,-1)  Tstring [] 0 

  return (Env ((emptyBlockEnv BTdecs):newBlockEnv:stack))


------------------------
-----MODALITY CHECK-----
------------------------
--controlla se la modalità della definizione e i parametri attuali della chiamata sono compatibili
checkModality::Env->Pident->[Exp]->Writer [String] ()
checkModality env pident callExprs = do
  --trova la posizione dei parametri
  posTypLs <- mapM (inferExpr env) callExprs --trova la lista di PosTyp
  posLs <- mapM (\(pos,typ) -> do return pos) posTypLs --Ritorna la lista di Pos dal PosTyp
  --trova le modalità dei parametri formali
  (pos,(typModLs,_,_))<-lookFunc pident env
  --struttura dati supporto
  --triples<- (zip3 posLs callExprs typModLs) --sostituito con riga 559
  --controlla che le espressioni siano lexpr in base alla modalità dei parametri
  let triples=zip3 posLs callExprs typModLs -- sostituisce riga 557 (triples<- etc.)
  mapM checkLexpr triples
  --controlla che non vengano passate costanti in una modalità che assegna un valore
  mapM (checkConstCall env) triples
  return ()

--controlla se il parametro attuale è una Lexpr se la modalità lo richiede
checkLexpr::(Pos,Exp,TypMod)->Writer [String] ()
checkLexpr (pos,expr,(typ,Just modal)) = do
  if  modalityRequiresLexpr modal
  then
    if isLexpr expr
    then return ()
    else do
      let ident=gettypid expr
      tell $[(show pos) ++ ":" ++ "Modality of Parameter "++ident++" requires an L-Expression"]
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

checkConstCall::Env->(Pos,Exp,TypMod)->Writer [String] ()
checkConstCall env (_,expr,(typ,Just modal)) = do
  case expr of
    Evar (pident@(Pident (pos,ident)))->do
      var<-lookVar pident env
      case var of
        (_,(_,(Just varmodal)))->do
          if (varmodal==Modality_CONST) && modalityRequiresLexpr modal
            then
              tell $ [show pos ++ ":" ++ "Cannot pass parameter "++ident++" by constant when Modality requires an L-Expression"]
            else
              return ()
        otherwise->return ()
    Arraysel exprArray _ -> do
      (pident@(Pident (pos,ident)))<- getVarFromArraySelection exprArray
      var<-lookVar pident env
      case var of
        (_,(_,varmod@(Just varmodal)))->do
          if varmodal==Modality_CONST && modalityRequiresLexpr modal
            then  tell $ [show pos ++":" ++ "Cannot pass an index of constant array "++ident++" when Modality requires an L-Expression"]
            else return ()
        otherwise->return ()
    otherwise->return ()

checkConstVar::Env->Exp->Writer [String] ()
checkConstVar env expr = do
  case expr of
    Evar (pident@(Pident (pos,ident)))->do
      var<-lookVar pident env
      case var of
        (_,(_,varmod@(Just varmodal)))->do
          if varmodal==Modality_CONST
            then  tell $ [show pos ++ ":" ++ "Cannot assign a value "++ident++ " to a CONST variable"]
            else return ()
        otherwise->return ()
    Arraysel exprArray _ -> do
      (pident@(Pident (pos,ident)))<- getVarFromArraySelection exprArray
      var<-lookVar pident env
      case var of
        (_,(_,varmod@(Just varmodal)))->do
          if varmodal==Modality_CONST
            then  tell $ [show pos ++ ":" ++ "Cannot assign a value "++ident++" to an index of a CONST array"]
            else return ()
        otherwise->return ()
    otherwise->return ()


getVarFromArraySelection :: Exp -> Writer [String] Pident
getVarFromArraySelection expr = case expr of
  Evar pident -> do
    return pident
  Arraysel exprArray _ -> getVarFromArraySelection exprArray
