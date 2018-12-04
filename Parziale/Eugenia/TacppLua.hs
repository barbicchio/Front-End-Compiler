{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module TacppLua where

import Prelude hiding((<>))
import TacLua
import AbsLua
import Text.PrettyPrint

tab = 5

instance TacPP TacInst where 
      prettyPrint tac = vcat . map prettyPrint $ tac

instance TacPP TAC where
      prettyPrint (TACSLabel label)                    = text "Start"<+>text label <> colon
      prettyPrint (TACELabel label)                    = text "End of"<+>text label
      prettyPrint (TACLabel label)                     = text label <> colon
      --prettyPrint (TACBinaryOp id left op right)     = nest tab $ text id <+> text "=" <+> text left <+> text op <+> text right
      prettyPrint (TACAssign id var)                  = nest tab $ text id <+> text "=" <+> text var
      prettyPrint (TACAssignCast id  typ var)         = nest tab $ text id <+> text "=" <+>text(show typ) <+> text var
      prettyPrint (TACBinaryInfixOp adr adr1 op adr2) =case op of
        ArithOp subop-> nest tab $ text adr<+>text"="<+>text adr1<+>text(show subop)<+> text adr2
        RelOp subop->case subop of
         Gt-> nest tab $ text adr<+>text"="<+>text adr1<+>text ">"<+> text adr2
         Lt-> nest tab $ text adr<+>text"="<+>text adr1<+>text "<"<+> text adr2
         Eq-> nest tab $ text adr<+>text"="<+>text adr1<+>text "=="<+> text adr2
         Neq->nest tab $ text adr<+>text"="<+>text adr1<+>text "~="<+> text adr2
         GtE->nest tab $ text adr<+>text"="<+>text adr1<+>text "=>"<+> text adr2
         LtE->nest tab $ text adr<+>text"="<+>text adr1<+>text "<="<+> text adr2
        BoolOp subop->case subop of
          And->nest tab $ text adr<+>text"="<+>text adr1<+>text "and"<+> text adr2
          Or->nest tab $ text adr<+>text"="<+>text adr1<+>text "or"<+> text adr2
      prettyPrint (TACBinaryInfixOpCast adr typ adr1 op adr2) =case op of
        ArithOp subop-> nest tab $ text adr<+>text"="<+>text(show typ)<+>text"("<+>text adr1<+>text(show subop)<+> text adr2<+>text")"
        RelOp subop->case subop of
         Gt-> nest tab $ text adr<+>text"="<+>text(show typ)<+>text adr1<+>text ">"<+> text adr2
         Lt-> nest tab $ text adr<+>text"="<+>text(show typ)<+>text adr1<+>text "<"<+> text adr2
         Eq-> nest tab $ text adr<+>text"="<+>text(show typ)<+>text adr1<+>text "=="<+> text adr2
         Neq->nest tab $ text adr<+>text"="<+>text(show typ)<+>text adr1<+>text "~="<+> text adr2
         GtE->nest tab $ text adr<+>text"="<+>text(show typ)<+>text adr1<+>text "=>"<+> text adr2
         LtE->nest tab $ text adr<+>text"="<+>text(show typ)<+>text adr1<+>text "<="<+> text adr2
        BoolOp subop->case subop of
         And->nest tab $ text adr<+>text"="<+>text(show typ)<+>text adr1<+>text "and"<+> text adr2
         Or->nest tab $ text adr<+>text"="<+>text(show typ)<+>text adr1<+>text "or"<+> text adr2   
      prettyPrint(TACNewTmpCast temp typ1 genTyp addr1) = nest tab $ text temp<+> text "="<+> text "Cast"<+>text (show typ1) <+> text "To"<+>text (show genTyp)<+>text"("<+>text addr1<+>text")"
      prettyPrint (TACInit typ id pos typexp exp)= case (typexp,exp) of
        (Nothing,Nothing)-> nest tab $ text(show typ)<+>text id <>text"_"<> text (show pos)
        (Just exptyp,Just exp)->nest tab $ text id<>text("_"++(show pos)) <+> text "="<+> text exptyp<+>text exp 
      prettyPrint (TACInitCast typ id pos exp)= case exp of
        Nothing-> nest tab $ text(show typ)<+>text id <>text"_"<> text (show pos)
        Just exp->nest tab $ text id<>text("_"++(show pos)) <+> text "="<+>text(show typ)<+>text"("<+>text exp <+>text")"
      prettyPrint(TACTmp id pos typ addr)              = case typ of
        Tpointer _ ->nest tab $ text id <>text"_"<> text (show pos)<+>text"= addr"<+>text addr
        otherwise->nest tab $ text id <>text"_"<> text (show pos)<+>text"="<+>text (show typ)<+>text addr
      prettyPrint (TACUnaryOp addr op addr1)           = case op of
        Neg->nest tab $ text addr <+> text "=" <+> text "-" <+> text addr1
        Logneg->nest tab $ text addr <+> text "=" <+> text "not" <+> text addr1
      prettyPrint (TACNewTemp addr typ id pos mod)=case (pos,mod) of
        (Just pos,Nothing)->case typ of
          Tpointer subtyp->nest tab$ text addr<+> text "="<+>text (show subtyp)<+>text"*"<+>text id<>text"_"<>text (show pos)
          otherwise->nest tab$ text addr<+> text "="<+>text (show typ)<+>text id<>text"_"<>text (show pos)
        (Just pos,Just mod1)->case typ of
          Tpointer subtyp-> case mod1 of
            Modality_RES->nest tab$ text addr<+> text "="<+>text (show subtyp)<+>text"*"<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            Modality_VALRES->nest tab$ text addr<+> text "="<+>text (show subtyp)<+>text"*"<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            otherwise->nest tab$ text addr<+> text "="<+>text (show subtyp)<+>text"*"<+>text id<>text"_"<>text (show pos)
          otherwise-> case mod1 of
            Modality_RES->nest tab$ text addr<+> text "="<+>text (show typ)<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            Modality_VALRES->nest tab$ text addr<+> text "="<+>text (show typ)<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            otherwise->nest tab$ text addr<+> text "="<+>text (show typ)<+>text id<>text"_"<>text (show pos)
        (Nothing,Just mod1)->case mod1 of
            Modality_RES->nest tab$ text addr<+> text "="<+>text (show typ)<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            Modality_VALRES->nest tab$ text addr<+> text "="<+>text (show typ)<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            otherwise->nest tab$ text addr<+> text "="<+>text (show typ)<+>text id<>text"_"<>text (show pos)
        otherwise->nest tab$ text addr<+> text "="<+>text (show typ)<+>text id
      prettyPrint (TACNewTempCall addr typ lab)=nest tab$ text addr<+> text "="<+>text (show typ)<+>text"callFunc"<+>text lab
      prettyPrint(TACIncrDecr addr1 addr2 prepostincr)= nest tab $ text addr1<+>text"="<+>text addr2<+>text(show prepostincr)<+> text"1"
      prettyPrint (TACArr addr1 offset addr2)  =nest tab$ text addr1<+>text"["<+>text (show offset)<+>text"]"<+>text"="<+>text addr2
      prettyPrint (TACGotoM lab)                     =case lab of
         Nothing->nest tab $ text "goto"
         Just label->nest tab $ text "goto" <+> text label
      prettyPrint (TACGoto lab)                     =nest tab $ text "goto" <+> text lab --da eliminare
      prettyPrint (TACJump addr1 addr2 op lab)   = case op of
       RelOp subop-> case (subop,addr2) of
        (Gt,addr2)->nest tab $ text "if"<+>text addr1<+>text ">"<+>text addr2<+>text "goto"<+>text lab
        (Lt,addr2)->nest tab $ text "if"<+>text addr1<+>text "<"<+>text addr2<+>text "goto"<+>text lab
        (Eq,addr2)->nest tab $ text "if"<+>text addr1<+>text "=="<+>text addr2<+>text "goto"<+>text lab
        (Neq,addr2)->nest tab $ text "if"<+>text addr1<+>text "~="<+>text addr2<+>text "goto"<+>text lab
        (GtE,addr2)->nest tab $ text "if"<+>text addr1<+>text "=>"<+>text addr2<+>text "goto"<+>text lab
        (LtE,addr2)->nest tab $ text "if"<+>text addr1<+>text "<="<+>text addr2<+>text "goto"<+>text lab
      prettyPrint(TACtf addr (Just lab) bool)=case bool of
        True->nest tab $ text "if"<+>text addr<+> text "goto"<+>text lab
        False->nest tab $ text "ifFalse"<+>text addr<+> text "goto"<+>text lab
      prettyPrint (TACRet addr)                   = nest tab $ text "return" <+> text addr
      prettyPrint (TACCall id npar)               = nest tab $ text "call" <+> text id <> text "/" <> text (show npar)
      prettyPrint (TACParam addr)                 = nest tab $ text "parameter"<+> text addr
      prettyPrint (TACPointer addr1 addr2)        = nest tab $ text addr1 <+> text "= addr &"<>text addr2
      prettyPrint (TACCopy id pos typ flag) =case flag of
        0->nest tab $ text"copyOf"<+>idpos<+>text "="<>text(show typ)<+>text "*"<+>idpos
        1->nest tab $ text "*"<+>idpos<+>text "="<>text(show typ)<+>text"copyOf"<+>idpos
       where idpos=text id<>text"_"<>text (show pos)

class TacPP a where
  prettyPrint :: a -> Doc