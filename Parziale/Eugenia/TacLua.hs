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
			  ttff::(Maybe Label,Maybe Label),
        next::Label,
        first::Bool
			    }
	deriving (Show)			

startState = TacM 0 0 [] (Nothing,Nothing) "" True

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

data TAC= TACAssign Addr Addr           --modificare tutto con le posizioni
	| TACBinaryInfixOp Addr Addr InfixOp Addr
	--{| TACBinaryArithOp Addr Addr ArithOp Addr--}
	| TACSLabel Label --inizio funzione e/o programma
	| TACELabel Label --fine funzione e/o programma
	| TACLabel Label --label generica
	| TACTmp Ident Pos Typ Addr
	| TACUnaryOp Addr Unary_Op Addr
	| TACNewTemp Addr Typ Ident (Maybe Pos)
	| TACIncrDecr Addr Addr IncrDecr
	| TACJump Addr Addr InfixOp Label
	| TACGoto Label
  | TACtf Addr (Maybe Label) Bool
	{--| TACWhile String String String
	--| TACIf TAC String String
	--| TACReturn String
	--| TACCall String String String
	--| TACParam String
	--}
	| TACPreamble String
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


createInitialEnv::Env->State TacM(Env) --TODO aggiornare con le altre funzioni
createInitialEnv (Env (current:stack)) = do
  newBlockEnv<-addFuncDec current "writeInt" (-1,-1) Tvoid [(Tint,Just Modality_VAL)] Nothing
  newBlockEnv<-addFuncDec newBlockEnv "readInt" (-1,-1)  Tint [] Nothing
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

gettypid::Exp->State TacM(String,String)   --forse meglio ritornare (Typ,String)
gettypid exp=case exp of
	Eint (Pint(_,id))-> return ("Int",id)
	Ebool (Pbool(_,id))-> return ("Bool",id)
	Estring (Pstring(_,id))-> return ("String",id)
	Efloat (Preal(_,id))-> return ("Float",id)
	Echar (Pchar(_,id))-> return ("Char",id)

opposite::RelOp->InfixOp
opposite op=case op of
	Eq-> RelOp Neq
	Neq-> RelOp Eq
	Lt->RelOp GtE
	GtE->RelOp Lt
	LtE->RelOp Gt
	Gt->RelOp LtE


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
    modify (\s ->s{code = (code s) ++ nxtinst})
    return ()
{--addTmp::(Ident,Pos)->Addr->State TacM()
addTmp idpos addr= do
    modify (\s@TacM{tacmap=m} -> s{tacmap=Map.insert idpos addr m})
    return ()
    --}
tacGenerator program = execState (genProgram program) startState

genProgram :: Program -> State TacM ()
genProgram (Progr decls) = do
    addTAC $ [TACSLabel "Program"]
    env<-createInitialEnv emptyEnv
    genDecls env decls
    addTAC $ [TACELabel "Program"]
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
               	(typexp,exp)<-gettypid exp
                addTAC $ [TACInit typ id pos (Just typexp) (Just exp)]
                return newEnv  
               False->do --se inizializzazione complessa,allora temporaneo. 
               	addr<-genexp env exp
                addTAC $ [TACTmp id pos typ addr]
                --addTmp (id,pos) addr --aggiungo alla mappa la coppia (id,pos)-indirizzo
                return newEnv
            Nothing-> do --caso dichiarazione senza
              addTAC $ [TACInit typ id pos Nothing Nothing] --se possibile migliorare il tipo che viene stampato (soprattutto per gli array)
              newEnv <- addDec env dec
              return newEnv
      Func retTyp ident params _ decstmts -> do   --TODO:se funzione restituisce tipo void devo aggiugere return void
          label <- newlabel
          pushEnv<-(pushNewBlocktoEnv env (BTfun retTyp))
          pushEnv<-addParams pushEnv params --porto dentro i parametri
          addTAC $ [TACSLabel label]
          (pushEnv,list)<-genDecStmts pushEnv decstmts   --genero codice body -dichiarazioni di fun 
          addTAC $ [TACELabel label]
          let (funcs,envs)=unzip list
          genFunDecls funcs envs
          return env
         
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

genStm::Env->Stm->State TacM (Maybe[(Dec,Env)]) --controllare come viene fatta codifica espressioni
genStm env stm= case stm of
    Assgn op lexp rexp-> case op of
        Assign->do
           addrlexp<-genlexp env lexp
           addrrexp<-genexp env rexp
           addTAC $ [TACAssign addrlexp addrrexp]
           return Nothing
        AssgnArith subop->do
           addr<-newtemp
           addrlexp<-genlexp env lexp
           addTAC $ [TACAssign addr addrlexp]
           addrrexp<-genexp env rexp
           addTAC $ [TACBinaryInfixOp addrlexp addr (ArithOp subop) addrrexp]
           return Nothing
        AssgnBool subop->do --TODO:completare
        	return Nothing
    Valreturn exp-> do
		addr<-genexp env exp
		return Nothing
    SExp exp-> do
		genexp env exp
		return Nothing
    SimpleIf exp decstms-> do --TODO:codificare le espressioni di if/while
		pushEnv<- pushNewBlocktoEnv env BTifEls
		(_,fundecs)<-genDecStmts pushEnv decstms
		return (Just fundecs)
    IfThElse exp decstms1 decstms2->do --TODO:codificare le espressioni di if/while
        pushEnvIf<-pushNewBlocktoEnv env BTifEls
        pushEnvElse<-pushNewBlocktoEnv env BTifEls
        (_,fundecs1)<-genDecStmts pushEnvIf decstms1
        (_,fundecs2)<-genDecStmts pushEnvElse decstms2
        return (Just (fundecs1++fundecs2))
    DoWhile decstms exp-> do --TODO:codificare le espressioni di if/while
        pushEnv<-pushNewBlocktoEnv env BTloop
        (_,fundecs)<-genDecStmts env decstms
        return (Just fundecs)
    While exp decstms-> do --TODO:codificare le espressioni di if/while
        pushEnv<-pushNewBlocktoEnv env BTloop
        (_,fundecs)<-genDecStmts env decstms
        return (Just fundecs)
genexp :: Env->Exp-> State TacM (Addr)
genexp env exp = case exp of 
  InfixOp op exp1 exp2 ->case op of
    ArithOp subop->genArithOp env exp1 exp2 op
    RelOp subop->genRelOp env exp1 exp2 subop
    BoolOp subop->do
      (tt,ff)<-gets ttff
      case (tt,ff) of
        (Nothing,Nothing)->do
          newtt<-newlabel
          newff<-newlabel
          modify (\s->s{ttff=(Just newtt,Just newff)})
        {--(Nothing,Just _)->do
          newtt<-newlabel
          modify (\s{ttff=(tval,fval)} -> s{ttff=(newtt,fval)})
        (Just _,Nothing)->do
          newff<-newlabel
          modify (\s{ttff=(tval,fval)} ->s{ttff=(tval,newff)})--}
        otherwise->return ()
      (Just tt,Just ff)<-gets ttff
      addr<-genBoolOp env exp1 exp2 op
      addTAC $ [TACLabel tt]++[TACLabel ff]
      return addr
  Unary_Op subop exp->genUnaryOp env subop exp
  PrePost prepost exp->case prepost of
		Pre op->do
			tmp<-newtemp
			addrlexp<-genlexp env exp
			addTAC $ [TACAssign tmp addrlexp]++[TACIncrDecr addrlexp tmp op]
			return addrlexp
		Post op->do
			addrlexp<-genlexp env exp
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
		addTAC $ [TACNewTemp addr typ id (Just pos)]
		return(addr)
  otherwise->genlexp env exp
genlexp::Env->Exp->State TacM(Addr)
genlexp env exp= case exp of
	Evar ident@(Pident(_,id))->do
		(pos,_)<-lookVar ident env
		let idpos=id++"_"++(show pos)
		--addTmp (id,pos) addr
		--addTAC $ [TACNewTemp addr typ id pos]
		return(idpos)
	{--Arraysel id exp->do
		(pos,(typ,_))<-lookVar id  env
--}

genArithOp::Env->Exp->Exp->InfixOp->State TacM (Addr)
genArithOp env exp1 exp2 op=do
        addr1<-genexp env exp1
        addr2<-genexp env exp2
        addr<-newtemp
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
      addr1<-genexp env exp
      addr<-newtemp
      addTAC $[TACUnaryOp addr op addr1]
      return addr

genRelOp::Env->Exp->Exp->RelOp->State TacM(Addr) --TODO:scrivere in modo che funzioni anche nelle guardie cicli
genRelOp env exp1 exp2 op= do
	addr1<-genexp env exp1
	addr2<-genexp env exp2
	lab1<-newlabel
	lab2<-newlabel
	addr<-newtemp
	let opp=opposite op
	addTAC $ [TACJump addr1 addr2 opp lab1]++[TACNewTemp addr Tbool "true" Nothing]++[TACGoto lab2]
	addTAC $ [TACLabel lab1]++[TACNewTemp addr Tbool "false" Nothing]++[TACLabel lab2]
	return addr
genBoolOp::Env->Exp->Exp->InfixOp->State TacM(Addr)
genBoolOp env exp1 exp2 op= case op of
  BoolOp subop->case subop of
    And->do
     (tt,ff)<-gets ttff
     newtt<-newlabel--B1.tt
     {--modify (\s{ttff=(tval,Just fval)}->s{ttff=(Just newtt,Just fval)})
     genexp env exp1
     addTAC $[TACLabel newtt]
     modify (\s->s{ttff=(tt,ff)}) 
     genexp env exp2
     return ""
    Or->do
     (tt,ff)<-gets ttff
     newff<-newlabel--B1.ff
     modify (\s{ttff=(tval,fval)}->s{ttff=(tval,newff)})
     genexp env exp1
     addTAC $ [TACLabel newff]
     modify (\s->s{ttff=(tt,ff)})
     genexp env exp2--}
     return ""
  otherwise->do
    return""

