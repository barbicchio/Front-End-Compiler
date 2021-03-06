{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Tacpp where

import TacLua
import Text.PrettyPrint

tab = 5

instance TacPP TacInst where 
      prettyPrint tac = vcat . map prettyPrint $ tac

instance TacPP TAC where
      prettyPrint (TACLabel label)                    = text label <> colon
      --prettyPrint (TACBinaryOp id left op right)      = nest tab $ text id <+> text "=" <+> text left <+> text op <+> text right
      prettyPrint (TACAssign id var)                  = nest tab $ text id <+> text "=" <+> text var
      prettyPrint (TACBinaryArithOp adr adr1 op adr2) = nest tab $ text adr<+>text"="<+>text adr1<+>text(show op)<+> text adr2
      --prettyPrint (TACInt num)                        = nest tab $ text (show num)
      prettyPrint (TACInit typ id pos typexp exp)= case (typexp,exp) of
        (Nothing,Nothing)-> nest tab $ text(show typ)<+>text id <>text"_"<> text (show pos)
        (Just exptyp,Just exp)->nest tab $ text id<>text("_"++(show pos)) <+> text "="<+> text exptyp<+>text exp 
      prettyPrint(TACTmp id pos addr)              = nest tab $ text id <>text"_"<> text (show pos)<+>text"="<+>text addr
      prettyPrint (TACUnaryOp addr op addr1)             = nest tab $ text addr <+> text "=" <+> text (show op) <+> text addr1
      prettyPrint(TACNewTemp addr typ id pos)         =nest tab$ text addr<+> text "="--<+>text (show typ)<+>text id<>text"_"<>text (show pos)
      --prettyPrint (TACCondition left op right)        = nest tab $ text left <> text op <> text right
      --prettyPrint (TACIf tacCondition l1 l2)          = nest tab $ text "if" <+> prettyPrint tacCondition <+> text "goto" <+> text l1 $$ text "goto" <+> text l2
      --prettyPrint (TACGoto label)                     = nest tab $ text "goto" <+> text label 
      --prettyPrint (TACReturn label)                   = nest tab $ text "return" <+> text label
      --prettyPrint (TACPreamble s)                     = nest tab $ text s
      --prettyPrint (TACParam label)                    = nest tab $ text "param" <+> text label
      --prettyPrint (TACCallVoid  id npar)              = nest tab $ text "Call" <+> text id <> text "," <> text npar
      --prettyPrint (TACCall var id npar)               = nest tab $ text var <+> text "=" <+> text "Call" <+> text id <> text "," <> text npar
     -- prettyPrint (TACException label)                = nest tab $ text "on exception goto" <+> text label

class TacPP a where
  prettyPrint :: a -> Doc