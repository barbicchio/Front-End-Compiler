{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintLua where

-- pretty-printer generated by the BNF converter

import AbsLua
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    --"{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    --"}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    --"}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    --";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print Pident where
  prt _ (Pident (_,i)) = doc (showString ( i))


instance Print Pint where
  prt _ (Pint (_,i)) = doc (showString ( i))


instance Print Pbool where
  prt _ (Pbool (_,i)) = doc (showString ( i))


instance Print Pstring where
  prt _ (Pstring (_,i)) = doc (showString ( i))


instance Print Preal where
  prt _ (Preal (_,i)) = doc (showString ( i))


instance Print Pchar where
  prt _ (Pchar (_,i)) = doc (showString ( i))



instance Print Program where
  prt i e = case e of
    Progr decs -> prPrec i 0 (concatD [prt 0 decs])

nl=doc (showString "\n")

instance Print Dec where
  prt i e = case e of
    --DecFunction functiondef -> prPrec i 0 (concatD [prt 0 functiondef])
    Func typespecifier pident parameters _ compoundstm->prPrec i 0 (concatD $ [ doc (showString "function"),prt 0 typespecifier,prt 0 pident,doc (showString "("),prt 0 parameters,doc (showString ")"),nl,prt 0 compoundstm,doc (showString "end"),nl])
    VarDeclar typespecifier pident exp -> prPrec i 0 (concatD $ [prt 0 typespecifier,prt 0 pident]++ prex++[nl])
       where prex=case exp of Nothing -> [] ; (Just exp) -> [doc (showString "="),doc (showString "("),prt 0 exp,doc (showString ")")]
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x,nl, prt 0 xs])
instance Print Type_specifier where
  prt i e = case e of
    Tchar -> prPrec i 0 (concatD [doc (showString "character")])
    Tint -> prPrec i 0 (concatD [doc (showString "integer")])
    Tbool -> prPrec i 0 (concatD [doc (showString "boolean")])
    Tstring -> prPrec i 0 (concatD [doc (showString "string")])
    Tfloat -> prPrec i 0 (concatD [doc (showString "float")])
    Tpointer typespecifier -> prPrec i 0 (concatD [doc (showString "pointer"), prt 0 typespecifier])
    Tarray num typespecifier -> prPrec i 0 (concatD $ [doc (showString "{")]++prex++[doc (showString "}"), prt 0 typespecifier])
        where prex=case num of Nothing -> [] ; (Just num) -> [prt 0 num]

instance Print Argument where
  prt i e = case e of
    FormPar modality typespecifier pident -> prPrec i 0 (concatD [prt 0 modality, prt 0 typespecifier, prt 0 pident])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Modality where
  prt i e = case e of
    Modality_VAL -> prPrec i 0 (concatD [doc (showString "val")])
    Modality_RES -> prPrec i 0 (concatD [doc (showString "res")])
    Modality_VALRES -> prPrec i 0 (concatD [doc (showString "valres")])
    Modality_NAME -> prPrec i 0 (concatD [doc (showString "name")])
    Modality_CONST -> prPrec i 0 (concatD [doc (showString "const")])
    Modality_REF -> prPrec i 0 (concatD [doc (showString "ref")])

instance Print Stm where
  prt i e = case e of
    Assgn assignmentop lexp exp -> prPrec i 0 (concatD [prt 0 lexp, prt 0 assignmentop, prt 0 exp,nl])
    SimpleIf exp chunkstm -> prPrec i 0 (concatD [doc (showString "if"), prt 0 exp, doc (showString "then"), prt 0 chunkstm, doc (showString "end")])
    IfThElse exp chunkstm1 chunkstm2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 exp, doc (showString "then"), prt 0 chunkstm1, doc (showString "else"), prt 0 chunkstm2, doc (showString "end")])
    While exp chunkstm -> prPrec i 0 (concatD [doc (showString "while"), prt 0 exp, doc (showString "do"), prt 0 chunkstm, doc (showString "end")])
    DoWhile chunkstm exp -> prPrec i 0 (concatD [doc (showString "repeat"), prt 0 chunkstm, doc (showString "until"), prt 0 exp])
    Valreturn returnstm -> prPrec i 0 (concatD [prt 0 returnstm])
    SExp exp -> prPrec i 0 (concatD [prt 0 exp])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print DecStm where
  prt i e = case e of
    Dec dec -> prPrec i 0 (concatD [prt 0 dec])
    Stmt stm -> prPrec i 0 (concatD [prt 0 stm])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print Exp where
  prt i e = case e of
    Fcall pident exps _ -> prPrec i 0 (concatD [prt 0 pident, doc (showString "("), prt 0 exps, doc (showString ")")])
    InfixOp infixop exp1 exp2-> prPrec i 0 (concatD [prt 0 exp1, prt 0 infixop, prt 0 exp2])
    Unary_Op unaryoperator exp -> prPrec i 15 (concatD [prt 0 unaryoperator, prt 14 exp])
    Efloat preal -> prPrec i 17 (concatD [prt 0 preal])
    Eint pint -> prPrec i 17 (concatD [prt 0 pint])
    Ebool pbool -> prPrec i 17 (concatD [prt 0 pbool])
    Estring pstring -> prPrec i 17 (concatD [prt 0 pstring])
    Echar pchar -> prPrec i 17 (concatD [prt 0 pchar])
    Lexp lexp -> prPrec i 17 (concatD [prt 0 lexp])
    Addr exp-> prPrec i 0 (concatD [doc (showString "&"), prt 14 exp])
    Evar pident -> prPrec i 0 (concatD [prt 0 pident])
    Indirection exp -> prPrec i 0 (concatD [doc (showString "*"), prt 0 exp])
    Arr exps->prPrec i 0 (concatD [doc (showString "{"),prt 0 exps,doc (showString "}")])
    Arraysel pident exp -> prPrec i 0 (concatD [prt 0 pident, doc (showString "{"), prt 0 exp, doc (showString "}")])
    PreIncr lexp -> prPrec i 0 (concatD [doc (showString "++"), prt 0 lexp])
    PreDecr lexp -> prPrec i 0 (concatD [doc (showString "--"), prt 0 lexp])
    PostIncr lexp -> prPrec i 0 (concatD [prt 0 lexp, doc (showString "++")])
    PostDecr lexp -> prPrec i 0 (concatD [prt 0 lexp, doc (showString "--")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print Unary_Op where
  prt i e = case e of
    Neg -> prPrec i 0 (concatD [doc (showString "-")])
    Logneg -> prPrec i 0 (concatD [doc (showString "not")])

instance Print Assignment_Op where
  prt i e = case e of
    Assign -> prPrec i 0 (concatD [doc (showString "=")])
    AssgnArith arithop->prPrec i 0 (concatD [prt 0 arithop,doc (showString "=")])
    AssgnBool boolop->prPrec i 0 (concatD [prt 0 boolop,doc (showString "=")])
instance Print InfixOp where
  prt i e = case e of
    ArithOp arithop->prPrec i 0 (concatD [prt 0 arithop])
    RelOp relop->prPrec i 0 (concatD [prt 0 relop])
    BoolOp boolop-> prPrec i 0 (concatD [prt 0 boolop])

instance Print ArithOp where
  prt i e = case e of
    Mul->prPrec i 0 (concatD [doc (showString "*")])
    Add->prPrec i 0 (concatD [doc (showString "+")])
    Sub->prPrec i 0 (concatD [doc (showString "-")])
    Div->prPrec i 0 (concatD [doc (showString "/")])
    Mod->prPrec i 0 (concatD [doc (showString "%")])
    Pow->prPrec i 0 (concatD [doc (showString "^")])

instance Print RelOp where
  prt i e = case e of
    Eq->prPrec i 0 (concatD [doc (showString "==")])
    Neq->prPrec i 0 (concatD [doc (showString "~=")])
    Lt->prPrec i 0 (concatD [doc (showString "<")])
    LtE->prPrec i 0 (concatD [doc (showString "<=")])
    Gt->prPrec i 0 (concatD [doc (showString ">")])
    GtE->prPrec i 0 (concatD [doc (showString ">=")])

instance Print BoolOp where
  prt i e = case e of
    Or->prPrec i 0 (concatD [doc (showString "or")])
    And->prPrec i 0 (concatD [doc (showString "and")])


