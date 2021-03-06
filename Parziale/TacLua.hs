module TacLua where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans.State
import AbsLua
import Debug.Trace

type TacInst=[TAC]

data TacM = TacM{
        kaddr::Int, --contatore temporanei
        klab::Int,  --contatore label
        code::TacInst, --TAC
        ttff::(Maybe Label,Maybe Label), --Etichette true,false
        first::Bool, --Se è falso sto valutando una guardia oppure una sottoespressione di un assegnamento di tipo booleano
        offset::Int, --offset array
        arrayinfo::(Maybe Pident,Maybe Typ,Maybe Typ) --(base,type,elemtype) dell'array corrente
        }
  deriving (Show)     

startState = TacM 0 0 [] (Nothing,Nothing) True 0 (Nothing,Nothing,Nothing)

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
-- trasformarlo in data |ArrAddr String Pos Int|PosAddr String Pos
type Label = String
type Mod = Maybe Modality

type Sigs =Map.Map Ident PosSig
type Context = Map.Map Ident PosTypMod
type TacContext = Map.Map (Ident,Pos) Addr

type Sig = (Typ,[TypMod],Label)
type Pos = (Int,Int)
type PosTyp=(Pos,Typ)
type PosTypMod = (Pos,TypMod)
type TypMod = (Typ,Mod)
type PosSig = (Pos,Sig)

data TAC= TACAssign Addr Addr          
  | TACAssignCast Addr Typ Addr         
  | TACBinaryInfixOp Addr Addr InfixOp Addr
  | TACBinaryInfixOpCast Addr Typ Addr InfixOp Addr
  | TACSLabel Label --inizio funzione e/o programma
  | TACELabel Label --fine funzione e/o programma
  | TACLabel Label --label generica
  | TACTmp Ident Pos Typ Addr --temporaneo relativo a left expression
  | TACUnaryOp Addr Unary_Op Addr
  | TACNewTemp Addr Typ Ident (Maybe Pos) Mod 
  | TACNewTempCall Addr Typ Label --temporaneo associato ad una chiamata a funzione
  | TACIncrDecr Addr Addr IncrDecr  --decrementi incrementi
  | TACNewTmpCast Addr Typ Typ Addr
  | TACJump Addr Addr InfixOp Label --salti per relop
  | TACCopy String Pos Typ Int --int 0 se entrata,1 se uscita
  | TACGoto Label
  | TACGotoM (Maybe Label)  --unificare queste due
  | TACtf Addr (Maybe Label) Bool --se sono in un ciclo salto se addr=valore Bool salto alla label
  | TACRet Addr 
  | TACExit --esco dalla funzione,potenzialmente estendibile.
  | TACInit Typ Ident Pos (Maybe String) (Maybe String)
  | TACInitCast Typ Ident Pos (Maybe String)
  | TACCall Label Int -- chiamata a procedura
  | TACArr Addr Int Addr
  | TACPointer Addr Addr 
  | TACParam Addr --parametro indirizzo
 deriving(Show)

--FCall,puntatori,relativa gestione parametri vari.
-------------------------------
--funzioni ausiliarie per ENV--
-------------------------------
addDec::Env->Dec->State TacM(Env) 
addDec env@(Env (current:stack)) dec = case dec of
  VarDeclar typ pident@(Pident (pos,ident)) exp ->do
    newBlockEnv<-addVarDec current ident pos typ Nothing
    return (Env (newBlockEnv:stack))
  Func typ pident@(Pident (pos,ident)) params nparams _  -> do
    let label=ident++(show pos)
    newBlockEnv<-addFuncDec current ident pos typ (getParamsModTyp params) label
    return (Env (newBlockEnv:stack))
--aggiunge una variabile a un contesto
addVarDec::BlockEnv->Ident->Pos->Typ->Mod->State TacM(BlockEnv)
addVarDec (BlockEnv sigs context blockTyp) ident pos@(line,col) typ mod= do
  record<-lookVarInContext ident context
  case record of
    Nothing -> return (BlockEnv sigs (Map.insert ident (pos,(typ,mod)) context) blockTyp)

--aggiunge una funzione a un contesto
addFuncDec::BlockEnv->Ident->Pos->Typ->[(Typ,Mod)]->Label->State TacM(BlockEnv)
addFuncDec (BlockEnv sigs context blockTyp)  ident pos@(line,col) returnTyp paramsTyp label = do
  record<-lookFuncInSigs ident sigs
  case record of
    Nothing -> return (BlockEnv (Map.insert ident (pos,(returnTyp,paramsTyp,label)) sigs) context blockTyp)

addParams::Env->[Argument]->State TacM(Env,[(String,Pos,Typ)])
addParams env parameters = foldM addParam (env,[]) parameters
 
addParam::(Env,[(String,Pos,Typ)])->Argument->State TacM(Env,[(String,Pos,Typ)]) --modalità diverse possono avere anche tac associato
addParam ((Env (current:stack)),list) pars = case pars of
    FormPar mod typ (Pident (pos,ident)) -> case mod of
      Modality_VALRES->do
           addTAC $ [TACCopy ident pos typ 0]
           newBlockEnv<-addVarDec current ident pos typ (Just mod)
           return ((Env (newBlockEnv:stack)),((ident,pos,typ):list))
      Modality_RES-> do
           newBlockEnv<-addVarDec current ident pos typ (Just mod)
           return ((Env (newBlockEnv:stack)),((ident,pos,typ):list))
      otherwise->do
           newBlockEnv<-addVarDec current ident pos typ (Just mod)
           return ((Env (newBlockEnv:stack)),list)

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
  newBlockEnv<-addFuncDec current "writeInt" (-1,-1) Tvoid [(Tint,Just Modality_VAL)] ("writeInt"++(show (-1,-1)))
  newBlockEnv <- addFuncDec newBlockEnv "writeFloat" (-1,-1) Tvoid [(Tfloat,Just Modality_VAL)] ("writeFloat"++(show (-1,-1)))
  newBlockEnv <- addFuncDec newBlockEnv "writeChar" (-1,-1) Tvoid [(Tchar,Just Modality_VAL)] ("writeChar"++(show (-1,-1)))
  newBlockEnv <- addFuncDec newBlockEnv "writeString" (-1,-1) Tvoid [(Tstring,Just Modality_VAL)] ("writeString"++(show (-1,-1)))

  newBlockEnv <- addFuncDec newBlockEnv "readInt" (-1,-1)  Tint [] ("readInt"++(show (-1,-1)))
  newBlockEnv <- addFuncDec newBlockEnv "readFloat" (-1,-1)  Tfloat [] ("readFloat"++(show (-1,-1)))
  newBlockEnv <- addFuncDec newBlockEnv "readChar" (-1,-1)  Tchar [] ("readChar"++(show (-1,-1)))
  newBlockEnv <- addFuncDec newBlockEnv "readString" (-1,-1)  Tstring [] ("readString"++(show (-1,-1)))
  return (Env ((emptyBlockEnv BTdecs):newBlockEnv:stack))

filterdecs::[Dec]->[Dec]
filterdecs []=[]

filterdecs (dec:decs)= case dec of
  Func _ _ _ _ _ ->dec: filterdecs decs
  otherwise->filterdecs decs

filterdecstmts :: [DecStm] -> [Dec]
filterdecstmts  [] = []
filterdecstmts  (decstm:decstmts) = case decstm of
  Dec dec@(Func _ _ _ _ _)  -> dec:(filterdecstmts decstmts)
  otherwise -> (filterdecstmts decstmts)

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

getdim::Typ->Int
getdim typ= case typ of
  Tint->4
  Tbool->1
  Tchar->1
  Tfloat->8
  Tstring->16
  Tarray _ subtyp->getdim subtyp
  Tpointer subtyp->getdim subtyp -- ??? controllare

gettyp::Typ->Typ
gettyp typ= case typ of
  Tpointer subtyp->gettyp subtyp
  Tarray _ subtyp->gettyp subtyp
  otherwise->typ

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
addTAC :: [TAC] -> State TacM()
addTAC nxtinst = do
    modify (\s ->s{code = (code s) ++ nxtinst})
    return ()
-------------------------------------
------generazione codice TAC---------
-------------------------------------
tacGenerator program = execState (genProgram program) startState

genProgram :: Program -> State TacM ()
genProgram (Progr decls) = do
    addTAC $ [TACSLabel "Program"]
    env<-createInitialEnv emptyEnv
    genDecls env decls
    addTAC $ [TACELabel "Program\n"]
    return ()

genDecls :: Env->[Dec] -> State TacM(Env)
genDecls env decs = do 
    newEnv<-foldM addDec env fundecs
    foldM genDecl newEnv decs
      where fundecs=filterdecs decs

genFunDecls::[Dec]->[Env]->State TacM()
genFunDecls [] []=return ()
genFunDecls (dec:decs) (env:envs)=do
            genDecl env dec
            genFunDecls decs envs
            return ()

genDecl :: (Env)->Dec-> State TacM (Env)
genDecl env dec = case dec of
      VarDeclar typ ident@(Pident(pos,id)) exp -> 
        case exp of 
            Just exp-> do
              newEnv <- addDec env dec 
              simplexp<-issimple exp
              case simplexp of --caso inizializzazione con semplice valore,no temporaneo. 
               True->do
                typ1<-inferExpr env exp
                (typexp,exp)<-gettypid exp
                if (typ/=typ1)
                then do
                genTyp<-genericType typ1 typ
                addTAC $ [TACInitCast  genTyp id pos (Just exp)]
                else addTAC $ [TACInit typ id pos (Just typexp) (Just exp)]
                return newEnv  
               False->do --se inizializzazione complessa,allora temporaneo. 
                case typ of
                 Tarray exps _ -> do
                  let elemtyp=gettyp typ
                  modify (\s->s{offset=0,arrayinfo=(Just ident,Just typ,Just elemtyp)})
                  genexp newEnv exp
                  modify (\s->s{offset=0,arrayinfo=(Nothing,Nothing,Nothing)})
                  return newEnv
                 otherwise->do 
                  addr<-genexp newEnv exp
                  addTAC $ [TACTmp id pos typ addr]
                  return newEnv
            Nothing-> do --caso dichiarazione senza inizializzazione
              addTAC $ [TACInit typ id pos Nothing Nothing] --se possibile migliorare il tipo che viene stampato (soprattutto per gli array)
              newEnv <- addDec env dec
              return newEnv
      Func retTyp ident@(Pident(pos,id)) params _ decstmts -> do  
          let label=id++(show pos)
          addTAC $ [TACSLabel label]
          pushEnv<-(pushNewBlocktoEnv env (BTfun retTyp))
          (pushEnv,copiedparams)<-addParams pushEnv params --env e eventuali parametri res/valres
          (pushEnv,list)<-genDecStmts pushEnv decstmts   --genero codice body escluse dichiarazioni di fun 
          mapM assigncopy (reverse copiedparams) --in presenza di parametri RES/VAL
          if(retTyp==Tvoid)
            then addTAC $ [TACRet "void"] --se non c'è tipo ritorno stampo return void
            else return()
          addTAC $ [TACELabel label]
          let (funcs,envs)=unzip list
          genFunDecls funcs envs --genero codice funzioni locali(lista al contrario mi pare)
          return env
            where assigncopy (ident,pos,typ) =do
                  addTAC $ [TACCopy ident pos typ 1]
         
genDecStmts::Env->[DecStm]->State TacM((Env),[(Dec,Env)])
genDecStmts env decstmts= do
    newEnv<-foldM addDec env fundecs
    (pushEnv,funcdecs)<-foldM genDecStm (newEnv,[]) decstmts
    return (pushEnv,funcdecs)
    where fundecs=filterdecstmts decstmts

genDecStm::(Env,[(Dec,Env)])->DecStm->State TacM(Env,[(Dec,Env)])
genDecStm (env,envdec) decstm= case decstm of
    Dec fundec@(Func _ _ _ _ _) -> do
        return (env,((fundec,env):envdec))
    Dec vardecl ->do
      pushEnv<-genDecl env vardecl
      return (pushEnv,envdec)
    Stmt stm -> do 
      fundecstm<-genStm env stm 
      case fundecstm of 
        Nothing->return (env,envdec)
        Just list->return (env,(list++envdec))

genStm::Env->Stm->State TacM (Maybe[(Dec,Env)])
genStm env stm= case stm of
    Assgn op lexp rexp-> case op of
        Assign->do
           addrlexp<-genlexp env lexp
           addrrexp<-genexp env rexp
           typlexp<-genlexpTyp env lexp
           {--case typlexp of --se ho qualcosa come g={0,1,2,3} devo poter calcolare offset --TODO
            Tarray exp _ ->do 
                 let elemtyp=gettyp typ
                 modify (\s->s{offset=0,arrayinfo=(Just ident,Just typ,Just elemtyp)})--}
           typrexp<-inferExpr env rexp
           if (typrexp /= typlexp) 
           then do
           genTyp<-generalize typrexp typlexp
           addTAC $ [TACAssignCast addrlexp genTyp addrrexp]
           else addTAC $ [TACAssign addrlexp addrrexp]
           return Nothing
        AssgnArith subop->do
           addr<-newtemp
           addrlexp<-genlexp env lexp
           typlexp<-genlexpTyp env lexp
           addTAC $ [TACAssign addr addrlexp]
           addrrexp<-genexp env rexp
           typrexp<-inferExpr env rexp
           if (typrexp /= typlexp) 
           then do
           genTyp<-generalize typrexp typlexp
           simplexp1<-issimple rexp
           case simplexp1 of 
            True->do 
              addTAC $ [TACBinaryInfixOp addrlexp addr (ArithOp subop) addrrexp]
              return Nothing
            False->do
              temp1<-newtemp
              addTAC $ [TACNewTmpCast temp1 typrexp genTyp addrrexp]
              temp2<-newtemp
              addTAC $ [TACBinaryInfixOpCast temp2 genTyp addr (ArithOp subop) temp1]
              return Nothing
           else do 
           addTAC $ [TACBinaryInfixOp addrlexp addr (ArithOp subop) addrrexp]
           return Nothing
        AssgnBool subop->do 
           addr<-newtemp
           addrlexp<-genlexp env lexp
           typlexp<-genlexpTyp env lexp
           addTAC $ [TACAssign addr addrlexp]
           addrrexp<-genexp env rexp
           typrexp<-inferExpr env rexp
           if (typrexp /= typlexp) 
           then do
           genTyp<-generalize typrexp typlexp
           addTAC $ [TACBinaryInfixOpCast addrlexp genTyp addr (BoolOp subop) addrrexp]
           else addTAC $ [TACBinaryInfixOp addrlexp addr (BoolOp subop) addrrexp]
           return Nothing
    Valreturn exp-> do --TODO: aggiungere allo stato una label per saltare all'eventuale postambolo della funzione "exit function"
        addr<-genexp env exp
        addTAC $[TACRet addr]        
        return Nothing
    SExp exp-> do
        genexp env exp
        return Nothing
    SimpleIf exp decstms-> do
        bodyif<-newlabel
        exitif<-newlabel
        modify(\s->s{ttff=(Just bodyif,Just exitif),first=False})
        pushEnv<- pushNewBlocktoEnv env BTifEls
        genexp env exp
        addTAC $ [TACLabel bodyif]
        modify (\s->s{first=True,ttff=(Nothing,Nothing)})
        (_,fundecs)<-genDecStmts pushEnv decstms
        addTAC $ [TACLabel exitif]
        return (Just fundecs)
    IfThElse exp decstms1 decstms2->do
        bodyif<-newlabel
        bodyelse<-newlabel
        next<-newlabel
        modify(\s->s{ttff=(Just bodyif,Just bodyelse),first=False})
        pushEnvIf<-pushNewBlocktoEnv env BTifEls
        genexp env exp
        modify(\s->s{ttff=(Nothing,Nothing),first=True})
        pushEnvElse<-pushNewBlocktoEnv env BTifEls
        addTAC $[TACLabel bodyif]
        (_,fundecs1)<-genDecStmts pushEnvIf decstms1
        addTAC $[TACGoto next]++[TACLabel bodyelse]
        (_,fundecs2)<-genDecStmts pushEnvElse decstms2
        addTAC $ [TACLabel next]
        return (Just (fundecs1++fundecs2))
    DoWhile decstms exp-> do
        bodywhile<-newlabel --B.tt
        next<-newlabel --B.ff
        pushEnv<-pushNewBlocktoEnv env BTloop
        addTAC $ [TACLabel bodywhile]
        (_,fundecs)<-genDecStmts pushEnv decstms
        modify(\s->s{ttff=(Just next,Just bodywhile),first=False}) --repeat B until E, if not E->loop,else exit loop
        genexp env exp
        modify(\s->s{ttff=(Nothing,Nothing),first=True})
        addTAC $ [TACLabel next]
        return (Just fundecs)
    While exp decstms-> do 
        bodywhile<-newlabel --B.tt
        next<-newlabel --S.next
        guard<-newlabel
        pushEnv<-pushNewBlocktoEnv env BTloop
        addTAC $[TACGoto guard]++[TACLabel bodywhile]
        (_,fundecs)<-genDecStmts pushEnv decstms
        modify(\s->s{ttff=(Just bodywhile,Just next),first=False})
        addTAC $ [TACLabel guard]
        genexp env exp
        modify(\s->s{ttff=(Nothing,Nothing),first=True})
        addTAC $[TACLabel next]
        return (Just fundecs)

genAssgnBool::Env->Exp->Exp->BoolOp->State TacM(Maybe[(Dec,Env)])
genAssgnBool env lexp rexp op=case op of
  And-> do  
      addrlexp<-genlexp env lexp
      tt<-newlabel
      ff<-newlabel
      next<-newlabel
      modify(\s->s{ttff=(Just tt,Just ff),first=False})
      addTAC $ [TACtf addrlexp (Just next) False]
      addrrexp<-genexp env rexp
      addr<-newtemp
      lassign<-newlabel
      addTAC $ [TACLabel tt]++[TACAssign addr "true"]++[TACGoto lassign]++[TACLabel ff]++[TACAssign addr "false"]
      addTAC $ [TACLabel lassign]++[TACAssign addrlexp addr]++[TACLabel next]
      modify(\s->s{ttff=(Nothing,Nothing),first=True})
      return Nothing
  Or->  do
      addrlexp<-genlexp env lexp
      tt<-newlabel
      ff<-newlabel
      next<-newlabel
      modify(\s->s{ttff=(Just tt,Just ff),first=False})
      addTAC $ [TACtf addrlexp (Just next) True]
      addrrexp<-genexp env rexp
      addr<-newtemp
      lassign<-newlabel
      addTAC $ [TACLabel ff]++[TACAssign addr "false"]++[TACGoto lassign]++[TACLabel tt]++[TACAssign addr "true"]
      addTAC $ [TACLabel lassign]++[TACAssign addrlexp addr]++[TACLabel next]
      modify(\s->s{ttff=(Nothing,Nothing),first=True})
      return Nothing

genexp :: Env->Exp-> State TacM (Addr)
genexp env exp = case exp of 
  InfixOp op exp1 exp2 ->case op of
    ArithOp subop->genArithOp env exp1 exp2 op
    RelOp subop->do
      first<-gets first
      case first of
       True->do
        (tt,ff)<-gets ttff
        case (tt,ff) of
         (Nothing,Nothing)->do
          newtt<-newlabel
          newff<-newlabel
          modify (\s->s{ttff=(Just newtt,Just newff)})
         (Nothing,Just _)->do
          newtt<-newlabel
          modify (\s-> s{ttff=(Just newtt,ff)})
         (Just _ ,Nothing)->do
          newff<-newlabel
          modify (\s->s{ttff=(tt,Just newff)})
         otherwise->return ()
        (Just tt,Just ff)<-gets ttff
        modify (\s->s{first=False})
        genRelOp env exp1 exp2 subop
        result<-newtemp
        next<-newlabel
        addTAC $ [TACLabel tt]++[TACNewTemp result Tbool "true" Nothing Nothing]++[TACGoto next] --se dentro guardia diverso...
        addTAC $ [TACLabel ff]++[TACNewTemp result Tbool "false" Nothing Nothing]++[TACLabel next]
        modify (\s->s{first=True,ttff=(Nothing,Nothing)})
        return result
       otherwise->genRelOp env exp1 exp2 subop
    BoolOp subop->do
      first<-gets first
      case first of
       True->do
        (tt,ff)<-gets ttff
        case (tt,ff) of
         (Nothing,Nothing)->do
          newtt<-newlabel
          newff<-newlabel
          modify (\s->s{ttff=(Just newtt,Just newff)})
         (Nothing,Just _)->do
          newtt<-newlabel
          modify (\s-> s{ttff=(Just newtt,ff)})
         (Just _ ,Nothing)->do
          newff<-newlabel
          modify (\s->s{ttff=(tt,Just newff)})
         otherwise->return ()
        (Just tt,Just ff)<-gets ttff
        modify (\s->s{first=False})
        genBoolOp env exp1 exp2 subop
        result<-newtemp
        next<-newlabel
        addTAC $ [TACLabel tt]++[TACNewTemp result Tbool "true" Nothing Nothing]++[TACGoto next] --se dentro guardia diverso...
        addTAC $ [TACLabel ff]++[TACNewTemp result Tbool "false" Nothing Nothing]++[TACLabel next]
        modify (\s->s{first=True,ttff=(Nothing,Nothing)})
        return result
       otherwise->genBoolOp env exp1 exp2 subop
  Unary_Op subop exp->genUnaryOp env subop exp
  PrePost prepost exp->case prepost of --controllare che secondo me fa casino
    Pre op->do
      tmp<-newtemp
      addrlexp<-genlexp env exp
      addTAC $ [TACAssign tmp addrlexp]++[TACIncrDecr addrlexp tmp op]
      return addrlexp
    Post op->do 
      addrlexp<-genlexp env exp 
      tmp<-newtemp --temporaneo con il valore prima dell'incremento/decremento
      addTAC $ [TACAssign tmp addrlexp]++[TACIncrDecr addrlexp addrlexp op]
      return tmp
  Fcall id@(Pident (pos,ident)) pars num-> do
    (pos,(retTyp,infoparams,lab))<-lookFunc id env
    let (_,modalityinfo)=unzip infoparams
    genparams env pars modalityinfo -- TODO: in questo caso gestire le diverse modalità parametri
    --(pos,(retTyp,_,lab))<-lookFunc id env
    case retTyp of
      Tvoid->do
       addTAC $ [TACCall lab num]
       return""
      otherwise->do
       addr<-newtemp
       addTAC$[TACNewTempCall addr retTyp lab]
       return addr
  Arr exp-> do
    genArr env exp
    modify (\s->s{offset=0,arrayinfo=(Nothing,Nothing,Nothing)})
    return ""
  Addr exp-> do
    addr<-genlexp env exp
    tmp<-newtemp
    addTAC$ [TACPointer tmp addr]
    return tmp
  Eint (Pint(_,num)) -> return num
  Efloat (Preal(_,num)) -> return num
  Ebool (Pbool(_,val))->return val
  Estring (Pstring(_,string))->return string
  Echar (Pchar(_,char))->return char
  Evar ident@(Pident(_,id))->do
    (pos,(typ,mod))<-lookVar ident env
    --let realtyp=gettyp typ  --tipo "base"
    addr<-newtemp
    addTAC $ [TACNewTemp addr typ id (Just pos) mod]
    return(addr)
  otherwise->genlexp env exp

genlexp::Env->Exp->State TacM(Addr) 
genlexp env exp= case exp of
  Evar ident@(Pident(_,id))->do
    (pos,(typ,mod))<-lookVar ident env
    let idpos=ifModreq++id++"_"++(show pos) --se modalità richiede faccio una copia
        ifModreq=case mod of
          Just Modality_RES->"copyOf"
          Just Modality_VALRES->"copyOf"
          Just Modality_REF->"ref "
          otherwise->""
    case typ of
      Tarray _ _ -> return idpos --TODO:se ho g={0,1,2,3} devo inizializzare l'offset somehow
      otherwise->return(idpos)
  Arraysel exp1 exp2->do
    genArrSel env exp1 exp2
  Indirection exp->do
    addr1<-genlexp env exp
    return ("*"++addr1)

genlexpTyp::Env->Exp->State TacM(Typ) 
genlexpTyp env exp = do
  typ<-inferExpr env exp
  return typ 

genexpTyp :: Env->Exp-> State TacM (Typ)
genexpTyp env exp = do
  typ<-inferExpr env exp
  return typ

genArrSel::Env->Exp->Exp->State TacM(Addr)
genArrSel env exp1 exp2= do
  let (Just id@(Pident(_,name)),exps)=getid exp1 []
      allexp=exps++[exp2] --aggiungo l'espressione più esterna
  (pos,(typ,_))<-lookVar id env
  let dims=getsizes typ
      dimtyp=getdim$gettyp typ
  offset<-getoffset env dimtyp allexp dims
  let addr=name++"_"++(show pos)++"["++offset++"]"
  return addr

inferExpr::Env->Exp->State TacM(Typ)
inferExpr env expr = case expr of

  Arr list@(exp:exprs)-> do
    typ<-inferExpr env exp
    return (Tarray Nothing typ) 

  InfixOp infixOp expr1 expr2 -> do
    typ<-inferInfixExpr env infixOp expr1 expr2
    return typ

  Unary_Op op  exp -> do
    typ<-inferUnaryExp env op exp
    return typ

  Addr exp -> do
    typ<-inferExpr env exp
    return (Tpointer typ)

  Indirection exp-> do
    typ<-inferExpr env exp
    case typ of
      Tpointer ptyp -> return ptyp

  Arraysel exprArray exprInt -> do 
    let (Just id@(Pident(_,name)),exps)=getid exprArray []
    (pos,(typ,_))<-lookVar id env
    return typ

  PrePost _ exp->do
    typ<-inferExpr env exp
    return typ

  Fcall pident@(Pident (pos,ident)) callExprs callNParams ->do
    (_,(retTyp,defParams,defNParams))<-lookFunc pident env --trova il tipo di ritorno 
    return retTyp

  Efloat (Preal (pos,val)) -> do
    return Tfloat
  Eint (Pint (pos,val)) -> do
    return Tint
  Ebool (Pbool (pos,val)) -> do
    return Tbool
  Estring (Pstring (pos,val)) -> do
    return Tstring
  Echar (Pchar (pos,val)) -> do
    return Tchar
  Evar pident@(Pident (pos,ident)) -> do
    (_,(typ,_))<-lookVar pident env
    return typ

inferInfixExpr::Env->InfixOp->Exp->Exp->State TacM(Typ)
inferInfixExpr env infixOp expr1 expr2 = do
  typ1<-inferExpr env expr1
  typ2<-inferExpr env expr2
  genTyp<-genericType typ1 typ2
  gtyp1<-generalize typ1 genTyp
  gtyp2<-generalize typ2 genTyp
  case infixOp of
    ArithOp op -> do
      return genTyp
    BoolOp op->do
      return Tbool
    RelOp op->do
      return Tbool

inferUnaryExp::Env->Unary_Op->Exp->State TacM(Typ)
inferUnaryExp env op expr = do
  typ<-inferExpr env expr
  return typ

generalize::Typ->Typ->State TacM(Typ)
generalize Tint Tfloat = do
 return Tfloat
generalize from to = do
 return from

genericType::Typ->Typ->State TacM(Typ)
genericType typ1 typ2 = do
  genTyp<-generalize typ2 typ1
  if genTyp==typ1
  then return genTyp
  else generalize typ1 typ2

getsizes::Typ->[Int] 
getsizes typ= case typ of
  Tarray (Just(Eint (Pint (_,num)))) subtyp->(read num:getsizes subtyp)
  otherwise->[]

getid::Exp->[Exp]->(Maybe Pident,[Exp]) --restituisce ident dell'array e le espressioni della selezione richiesta
getid exp list= case exp of
 Evar ident->(Just ident,list)
 Arraysel subexp indexexp->getid subexp (indexexp:list)
 
getoffset::Env->Int->[Exp]->[Int]->State TacM(Addr)
getoffset env dimtyp allexp dims = do
  addrs<-listaddr env dimtyp allexp dims []
  addr<-sumaddr (reverse addrs)
  return addr

listaddr::Env->Int->[Exp]->[Int]->[Addr]->State TacM([Addr])
listaddr _ _ [] _ list=return list
listaddr env size (exp:exps) (_:dims) list=do
  addr1<-genexp env exp
  let addr2=show $ foldl (*) size dims
  tmp<-newtemp
  addTAC $ [TACBinaryInfixOp tmp addr1 (ArithOp Mul) addr2]
  listaddr env size exps dims (tmp:list)

sumaddr::[Addr]->State TacM(Addr)
sumaddr (addr:[])=return addr
sumaddr (addr1:addr2:other)= do
  tmp<-newtemp
  addTAC $ [TACBinaryInfixOp tmp addr1 (ArithOp Add) addr2]
  sumaddr(tmp:other)

genparams::Env->[Exp]->[Mod]->State TacM ()
genparams _ [] []=return()
genparams env (exp:exps) (mod:mods)= do
        addr<-genexp env exp
        --addTAC $ [TACAssign par addr]
        addTAC $ [TACParam addr]
        genparams env exps mods
        return()

genArr::Env->[Exp]->State TacM ()  --TODO:se avanza tempo aggiungere i tipi a dx dell' =
genArr env []=return()
genArr env (exp:exps)=case exp of
  Arr subexp->do
    genArr env subexp
    genArr env exps
    return()
  otherwise->do
    (Just id@(Pident (pos,ident)),(Just typ),(Just subtyp))<-gets arrayinfo
    currentoff<-gets offset
    let arrayaddr=ident++"_"++(show pos)
    addr<-genexp env exp
    addTAC $ [TACArr arrayaddr currentoff addr]
    modify (\s->s{offset=currentoff+(getdim typ)}) 
    genArr env exps
    return()

genArithOp::Env->Exp->Exp->InfixOp->State TacM (Addr)
genArithOp env exp1 exp2 op=do
        addr1<-genexp env exp1
        typ1<-inferExpr env exp1
        addr2<-genexp env exp2
        typ2<-inferExpr env exp2
        addr<-newtemp
        if(typ1/=typ2)
         then do
        genTyp<-genericType typ1 typ2
        temp<-newtemp
        if(typ1/=genTyp)
        then do 
        simplexp1<-issimple exp1
        case simplexp1 of 
          True->do 
            addTAC $ [TACBinaryInfixOpCast addr genTyp addr1 op addr2]
            return addr
          False->do
            addTAC $ [TACNewTmpCast addr typ1 genTyp addr1]
            addTAC $ [TACBinaryInfixOpCast temp genTyp addr op addr2]
            return temp
        else do 
        simplexp2<-issimple exp2
        case simplexp2 of 
          True->do
            addTAC $ [TACBinaryInfixOpCast addr genTyp addr1 op addr2]
            return addr
          False->do
            addTAC $ [TACNewTmpCast addr typ2 genTyp addr2]
            addTAC $ [TACBinaryInfixOpCast temp genTyp addr1 op addr]
            return temp
        else do 
        addTAC $ [TACBinaryInfixOp addr addr1 op addr2]
        return(addr)
genUnaryOp :: Env->Unary_Op->Exp-> State TacM (Addr)
genUnaryOp env op exp = case op of
    Neg->do
      addr1<-genexp env exp
      addr<-newtemp
      addTAC $[TACUnaryOp addr op addr1]
      return addr
    Logneg->do 
      first<-gets first   
      case first of
        True->do
          addr1<-genexp env exp
          addr<-newtemp
          addTAC $[TACUnaryOp addr op addr1]
          return addr
        False->do
          (tt,ff)<-gets ttff
          modify (\s->s{ttff=(ff,tt)})
          addr1<-genexp env exp
          case addr1 of
            "true"->addTAC $ [TACGotoM ff]
            "false"->addTAC $ [TACGotoM tt]
            otherwise->return()
          case exp of
           Evar _ ->addTAC$[TACtf addr1 ff True] ++[TACtf addr1 tt False] --controllare tac pp
           otherwise->return()
          modify (\s->s{ttff=(tt,ff)})
          return ""

genRelOp::Env->Exp->Exp->RelOp->State TacM(Addr) 
genRelOp env exp1 exp2 op= do
  (Just tt,Just ff)<-gets ttff
  addr1<-genexp env exp1
  addr2<-genexp env exp2
  typ1<-inferExpr env exp1
  typ2<-inferExpr env exp2
  if(typ1/=typ2)
    then do
      genTyp<-genericType typ1 typ2
      temp<-newtemp
      if(typ1/=genTyp)
        then do 
        addTAC $ [TACNewTmpCast temp typ1 genTyp addr1]
        addTAC $ [TACJump temp addr2 (RelOp op) tt]++[TACGoto ff] 
      else do 
        addTAC $ [TACNewTmpCast temp typ2 genTyp addr2]
        addTAC $ [TACJump addr1 temp (RelOp op) tt]++[TACGoto ff] 
  else addTAC $ [TACJump addr1 addr2 (RelOp op) tt]++[TACGoto ff]
  return ""
genBoolOp::Env->Exp->Exp->BoolOp->State TacM(Addr)
genBoolOp env exp1 exp2 op= case op of
    And->do
     (tt,Just ff)<-gets ttff
     newtt<-newlabel--B1.tt
     modify (\s->s{ttff=(Just newtt,Just ff)})
     addr1<-genexp env exp1
     case addr1 of
      ""->return()
      "false"->addTAC $ [TACGoto ff]
      "true"->return()
      otherwise->addTAC $ [TACtf addr1 (Just ff) False]
     addTAC $[TACLabel newtt] --lab(B1.tt)
     modify (\s->s{ttff=(tt,Just ff)}) 
     addr2<-genexp env exp2
     case addr2 of
      ""->return()
      "false"->addTAC $ [TACGoto ff]
      "true"->return()
      otherwise->addTAC $ [TACtf addr2 (Just ff) False]++[TACGotoM tt]
     return ""
    Or->do
     (Just tt,ff)<-gets ttff
     newff<-newlabel--B1.ff
     modify (\s->s{ttff=(Just tt,Just newff)})
     addr1<-genexp env exp1
     case addr1 of
      ""->return()
      "false"->return()
      "true"->addTAC $ [TACGoto tt]
      otherwise->addTAC $ [TACtf addr1 (Just tt) True]
     addTAC $[TACLabel newff] --lab(B1.tt)
     modify (\s->s{ttff=(Just tt,ff)}) 
     addr2<-genexp env exp2
     case addr2 of
      ""->return()
      "false"->addTAC $ [TACGoto tt]
      "true"->return()
      otherwise->addTAC $ [TACtf addr2 (Just tt) True]++[TACGotoM ff]
     return ""