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

      --prettyPrint (TACInt num)                        = nest tab $ text (show num)
      prettyPrint (TACInit typ id pos typexp exp)= case (typexp,exp) of
        (Nothing,Nothing)-> nest tab $ text(show typ)<+>text id <>text"_"<> text (show pos)
        (Just exptyp,Just exp)->nest tab $ text id<>text("_"++(show pos)) <+> text "="<+> text exptyp<+>text exp 
      prettyPrint(TACTmp id pos typ addr)              = nest tab $ text id <>text"_"<> text (show pos)<+>text"="<+>text (show typ)<+>text addr
      prettyPrint (TACUnaryOp addr op addr1)           = case op of
        Neg->nest tab $ text addr <+> text "=" <+> text "-" <+> text addr1
        Logneg->nest tab $ text addr <+> text "=" <+> text "not" <+> text addr1
      prettyPrint (TACNewTemp addr typ id pos)=case pos of
        Just pos->nest tab$ text addr<+> text "="<+>text (show typ)<+>text id<>text"_"<>text (show pos)
        otherwise->nest tab$ text addr<+> text "="<+>text (show typ)<+>text id
      prettyPrint(TACIncrDecr addr1 addr2 prepostincr)= nest tab $ text addr1<+>text"="<+>text addr2<+>text(show prepostincr)<+> text"1"
      --prettyPrint (TACCondition left op right)        = nest tab $ text left <> text op <> text right
      --prettyPrint (TACIf tacCondition l1 l2)          = nest tab $ text "if" <+> prettyPrint tacCondition <+> text "goto" <+> text l1 $$ text "goto" <+> text l2
      prettyPrint (TACGoto label)                     = nest tab $ text "goto" <+> text label 
      prettyPrint (TACGotoM lab)                     =case lab of
         Nothing->nest tab $ text "goto"
         Just label->nest tab $ text "goto" <+> text label 
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
     -- prettyPrint (TACException label)                = nest tab $ text "on exception goto" <+> text label

class TacPP a where
  prettyPrint :: a -> Doc