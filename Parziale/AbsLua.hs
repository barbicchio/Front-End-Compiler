module AbsLua where

-- Haskell module generated by the BNF converter

newtype Pident = Pident ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype Pint = Pint ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype Pbool = Pbool ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype Pstring = Pstring ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype Preal = Preal ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype Pchar = Pchar ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

data Program = Progr [Dec]
  deriving (Eq, Ord, Show, Read)

data Dec
    = Func Type_specifier Pident [Argument] Int [DecStm]
    | VarDeclar Type_specifier Pident (Maybe Exp)
  deriving (Eq, Ord, Show, Read)

data Type_specifier
    = Tbool
    | Tchar
    | Tfloat
    | Tint
    | Tstring
    | Tvoid
    | Terror
    | Tarray (Maybe Exp) Type_specifier
    | Tpointer Type_specifier
  deriving (Eq, Ord, Show, Read)

data Argument = FormPar Modality Type_specifier Pident
  deriving (Eq, Ord, Show, Read)

data Modality
    = Modality_VAL
    | Modality_RES
    | Modality_VALRES
    | Modality_NAME
    | Modality_CONST
    | Modality_REF
  deriving (Eq, Ord, Show, Read)

data Stm
    = Assgn Assignment_Op Exp Exp
    | Valreturn Exp
    | SExp Exp
    | SimpleIf Exp [DecStm]
    | IfThElse Exp [DecStm] [DecStm]
    | While Exp [DecStm]
    | DoWhile [DecStm] Exp
  deriving (Eq, Ord, Show, Read)

data DecStm = Dec Dec | Stmt Stm
  deriving (Eq, Ord, Show, Read)

data Exp
    =InfixOp InfixOp Exp Exp
    | Unary_Op Unary_Op  Exp
    | Lexp Exp
    | Fcall Pident [Exp] Int
    | Efloat Preal
    | Eint Pint
    | Ebool Pbool
    | Estring Pstring
    | Echar Pchar
    | Addr Exp
    | Arr [Exp]
    | PrePost PrePost Exp
    --L expression
    | Evar Pident
    | Indirection Exp
    | Arraysel Exp Exp
  deriving (Eq, Ord, Show, Read)

data InfixOp = ArithOp ArithOp| RelOp RelOp | BoolOp BoolOp
 deriving (Eq, Ord, Show, Read)
data PrePost = Pre IncrDecr|Post IncrDecr
 deriving (Eq, Ord, Show, Read)
data IncrDecr = Incr|Decr
 deriving (Eq, Ord, Show, Read)
data ArithOp = Add|Sub|Mul|Div|Divint|Mod|Pow
 deriving (Eq, Ord, Show, Read)
data RelOp = Eq|Neq|Lt|LtE|Gt|GtE
  deriving (Eq, Ord, Show, Read)
data BoolOp = And|Or
  deriving (Eq, Ord, Show, Read)
data Unary_Op = Neg | Logneg
  deriving (Eq, Ord, Show, Read)

data Assignment_Op
    = Assign| AssgnArith ArithOp| AssgnBool BoolOp
  deriving (Eq, Ord, Show, Read)

