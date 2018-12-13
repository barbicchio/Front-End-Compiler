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
      prettyPrint (TACDLabel label int)= case int of
        1->text "End of"<+>text label
        0->text "Start of"<+>text label <> colon
      prettyPrint (TACLabel label)                     = text label <> colon
      --prettyPrint (TACBinaryOp id left op right)     = nest tab $ text id <+> text "=" <+> text left <+> text op <+> text right
      prettyPrint (TACNewTemp addr1 addr2)             = nest tab $ printAddr addr1 <+> text "=" <+> printAddr addr2
      prettyPrint (TACAssignCast id  typ var)         = nest tab $ printAddr id <+> text "=" <+>text(show typ) <+> printAddr var
      prettyPrint (TACBinaryInfixOp adr typ adr1 op adr2) =case op of
        ArithOp subop-> nest tab $ printAddr adr<+>text"="<+>text(show typ)<+>text"("<+>printAddr adr1<+>text(show subop)<+>text "_"<+>text(show typ)<+>printAddr adr2<+>text")"
        RelOp subop->case subop of
         Gt-> nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text ">"<+> printAddr adr2
         Lt-> nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text "<"<+> printAddr adr2
         Eq-> nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text "=="<+> printAddr adr2
         Neq->nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text "~="<+> printAddr adr2
         GtE->nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text "=>"<+> printAddr adr2
         LtE->nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text "<="<+> printAddr adr2
        BoolOp subop->case subop of
         And->nest tab $ printAddr adr<+>text"="<+>text(show typ)<+>printAddr adr1<+>text "and"<+> printAddr adr2
         Or->nest tab $ printAddr adr<+>text"="<+>text(show typ)<+>printAddr adr1<+>text "or"<+> printAddr adr2   
      prettyPrint (TACBinaryInfixOpCast adr typ adr1 op adr2) =case op of
        ArithOp subop-> nest tab $ printAddr adr<+>text"="<+>text(show typ)<+>text"("<+>printAddr adr1<+>text(show subop)<+>text "_"<+>text(show typ)<+>printAddr adr2<+>text")"
        RelOp subop->case subop of
         Gt-> nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text ">"<+> printAddr adr2
         Lt-> nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text "<"<+> printAddr adr2
         Eq-> nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text "=="<+> printAddr adr2
         Neq->nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text "~="<+> printAddr adr2
         GtE->nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text "=>"<+> printAddr adr2
         LtE->nest tab $ printAddr  adr<+>text"="<+>text(show typ)<+>printAddr  adr1<+>text "<="<+> printAddr adr2
        BoolOp subop->case subop of
         And->nest tab $ printAddr adr<+>text"="<+>text(show typ)<+>printAddr adr1<+>text "and"<+> printAddr adr2
         Or->nest tab $ printAddr adr<+>text"="<+>text(show typ)<+>printAddr adr1<+>text "or"<+> printAddr adr2   
      prettyPrint(TACNewTmpCast temp typ1 genTyp addr1) = nest tab $ printAddr temp<+> text "="<+>text (show genTyp)<+>text "convert"<+>text (show typ1) <+> text "_To_"<+>text (show genTyp)<+>text"("<+>printAddr addr1<+>text")"
      prettyPrint (TACInit typ id pos typexp addr)= case (typexp,addr) of
        (Nothing,Nothing)-> nest tab $ text(show typ)<+>text id <>text"_"<> text (show pos)
        (Just exptyp,Just addr)->nest tab $ text id<>text("_"++(show pos)) <+> text "="<+> text (show typ) <+>printAddr addr 
        (_,Just addr)->nest tab $ text id<>text("_"++(show pos)) <+> text "="<+>printAddr addr
      prettyPrint (TACInitCast typ id pos exp)= case exp of
        Nothing-> nest tab $ text(show typ)<+>text id <>text"_"<> text (show pos)
        Just exp->nest tab $ text id<>text("_"++(show pos)) <+> text "="<+>text(show typ)<+>text"("<+>text exp <+>text")"
      prettyPrint(TACTmp id pos typ addr)              = case typ of
        Tpointer _ ->nest tab $ text id <>text"_"<> text (show pos)<+>text"= addr"<+>printAddr addr
        otherwise->nest tab $ text id <>text"_"<> text (show pos)<+>text"="<+>text (show typ)<+>printAddr addr
      prettyPrint (TACUnaryOp addr op addr1)           = case op of
        Neg->nest tab $ printAddr addr <+> text "=" <+> text "-" <+> printAddr addr1
        Logneg->nest tab $ printAddr addr <+> text "=" <+> text "not" <+> printAddr addr1
      prettyPrint (TACAssign addr typ addr1)= case typ of
        Tpointer subtyp->nest tab $ printAddr addr<+> text "=" <+>text (show subtyp)<+>text"*"<>printAddr addr1
        otherwise->nest tab $ printAddr addr<+> text "=" <+>text (show typ)<+>printAddr addr1
      {--prettyPrint (TACNewTemp addr typ id pos mod)=case (pos,mod) of
        (Just pos,Nothing)->case typ of
          Tpointer subtyp->nest tab$ printAddr addr<+> text "="<+>text (show subtyp)<+>text"*"<+>text id<>text"_"<>text (show pos)
          otherwise->nest tab$ printAddr addr<+> text "="<+>text (show typ)<+>text id<>text"_"<>text (show pos)
        (Just pos,Just mod1)->case typ of
          Tpointer subtyp-> case mod1 of
            Modality_RES->nest tab$ printAddr addr<+> text "="<+>text (show subtyp)<+>text"*"<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            Modality_VALRES->nest tab$ printAddr addr<+> text "="<+>text (show subtyp)<+>text"*"<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            otherwise->nest tab$ printAddr addr<+> text "="<+>text (show subtyp)<+>text"*"<+>text id<>text"_"<>text (show pos)
          otherwise-> case mod1 of
            Modality_RES->nest tab$ printAddr addr<+> text "="<+>text (show typ)<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            Modality_VALRES->nest tab$ printAddr addr<+> text "="<+>text (show typ)<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            otherwise->nest tab$ printAddr addr<+> text "="<+>text (show typ)<+>text id<>text"_"<>text (show pos)
        (Nothing,Just mod1)->case mod1 of
            Modality_RES->nest tab$ printAddr addr<+> text "="<+>text (show typ)<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            Modality_VALRES->nest tab$ printAddr addr<+> text "="<+>text (show typ)<+>text"copyOf"<+>text id<>text"_"<>text (show pos)
            otherwise->nest tab$ printAddr addr<+> text "="<+>text (show typ)<+>text id<>text"_"<>text (show pos)
        otherwise->nest tab$ printAddr addr<+> text "="<+>text (show typ)<+>text id
      --}
      prettyPrint (TACNewTempCall addr typ lab)=nest tab$ printAddr addr<+> text "="<+>text (show typ)<+>text"callFunc"<+>text lab
      prettyPrint (TACArr addr1 offset addr2)  =nest tab$ printAddr addr1<+>text"["<+>text (show offset)<+>text"]"<+>text"="<+>printAddr addr2
      prettyPrint (TACGotoM lab)                     =case lab of
         Nothing->nest tab $ text "goto"
         Just label->nest tab $ text "goto" <+> text label
      prettyPrint (TACJump addr1 addr2 op lab)   = case op of
       RelOp subop-> case (subop,addr2) of
        (Gt,addr2)->nest tab $ text "if"<+>printAddr  addr1<+>text ">"<+>printAddr  addr2<+>text "goto"<+>text lab
        (Lt,addr2)->nest tab $ text "if"<+>printAddr  addr1<+>text "<"<+>printAddr  addr2<+>text "goto"<+>text lab
        (Eq,addr2)->nest tab $ text "if"<+>printAddr  addr1<+>text "=="<+>printAddr  addr2<+>text "goto"<+>text lab
        (Neq,addr2)->nest tab $ text "if"<+>printAddr  addr1<+>text "~="<+>printAddr  addr2<+>text "goto"<+>text lab
        (GtE,addr2)->nest tab $ text "if"<+>printAddr  addr1<+>text "=>"<+>printAddr  addr2<+>text "goto"<+>text lab
        (LtE,addr2)->nest tab $ text "if"<+>printAddr  addr1<+>text "<="<+>printAddr  addr2<+>text "goto"<+>text lab
      prettyPrint(TACtf addr (Just lab) bool)=case bool of
        True->nest tab $ text "if"<+>printAddr addr<+> text "goto"<+>text lab
        False->nest tab $ text "ifFalse"<+>printAddr addr<+> text "goto"<+>text lab
      prettyPrint (TACRet addr)                   = nest tab $ text "return" <+> printAddr addr
      prettyPrint (TACCall id npar)               = nest tab $ text "call" <+> text id <> text "/" <> text (show npar)
      prettyPrint (TACParam addr)                 = nest tab $ text "parameter"<+> printAddr addr
      prettyPrint (TACDeref addr1 addr2)        = nest tab $ printAddr addr1 <+> text "= addr &"<>printAddr addr2
      prettyPrint (TACStrLab lab string)= text lab<+>(nest tab $ text string)
printAddr::Addr->Doc
printAddr addr= case addr of
  SAddr subaddr->text subaddr
  PointAddr subaddr ->text "addr"<+> printAddr subaddr
  RefAddr subaddr ->text "*"<+> printAddr subaddr
  PosAddr subaddr pos mod -> case mod of
    Just Modality_REF-> text "*"<>text subaddr<>text "_"<>printPos pos
    otherwise->text subaddr<>text "_"<>printPos pos
      where maybepos=printPos
  ArrAddr subaddr pos offset-> text subaddr<>text "_"<>text (show pos)<>text "["<>text offset<>text "]"

printPos::Maybe Pos->Doc
printPos pos= case pos of
  Just pos1 -> text(show pos1)
  otherwise -> text ""
class TacPP a where
  prettyPrint :: a -> Doc