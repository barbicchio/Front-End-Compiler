-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParLua where
import AbsLua
import LexLua
import ErrM

}

%name pProgram Program
%name pDec Dec
%name pStm Stm
%name pExp Exp
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '%' { PT _ (TS _ 1) }
  '%=' { PT _ (TS _ 2) }
  '&' { PT _ (TS _ 3) }
  '&=' { PT _ (TS _ 4) }
  '(' { PT _ (TS _ 5) }
  ')' { PT _ (TS _ 6) }
  '*' { PT _ (TS _ 7) }
  '*=' { PT _ (TS _ 8) }
  '+' { PT _ (TS _ 9) }
  '++' { PT _ (TS _ 10) }
  '+=' { PT _ (TS _ 11) }
  ',' { PT _ (TS _ 12) }
  '-' { PT _ (TS _ 13) }
  '--' { PT _ (TS _ 14) }
  '-=' { PT _ (TS _ 15) }
  '/' { PT _ (TS _ 16) }
  '/=' { PT _ (TS _ 17) }
  '<' { PT _ (TS _ 18) }
  '<=' { PT _ (TS _ 19) }
  '=' { PT _ (TS _ 20) }
  '==' { PT _ (TS _ 21) }
  '>' { PT _ (TS _ 22) }
  '>=' { PT _ (TS _ 23) }
  '^' { PT _ (TS _ 24) }
  '^=' { PT _ (TS _ 25) }
  'and' { PT _ (TS _ 26) }
  'boolean' { PT _ (TS _ 27) }
  'character' { PT _ (TS _ 28) }
  'const' { PT _ (TS _ 29) }
  'do' { PT _ (TS _ 30) }
  'else' { PT _ (TS _ 31) }
  'end' { PT _ (TS _ 32) }
  'float' { PT _ (TS _ 33) }
  'function' { PT _ (TS _ 34) }
  'if' { PT _ (TS _ 35) }
  'integer' { PT _ (TS _ 36) }
  'name' { PT _ (TS _ 37) }
  'not' { PT _ (TS _ 38) }
  'or' { PT _ (TS _ 39) }
  'pointer' { PT _ (TS _ 40) }
  'ref' { PT _ (TS _ 41) }
  'repeat' { PT _ (TS _ 42) }
  'res' { PT _ (TS _ 43) }
  'return' { PT _ (TS _ 44) }
  'string' { PT _ (TS _ 45) }
  'then' { PT _ (TS _ 46) }
  'until' { PT _ (TS _ 47) }
  'val' { PT _ (TS _ 48) }
  'valres' { PT _ (TS _ 49) }
  'void' { PT _ (TS _ 50) }
  'while' { PT _ (TS _ 51) }
  '{' { PT _ (TS _ 52) }
  '{}' { PT _ (TS _ 53) }
  '|=' { PT _ (TS _ 54) }
  '}' { PT _ (TS _ 55) }
  '~=' { PT _ (TS _ 56) }


L_integ  { PT _ (TI $$) }
L_Pident { PT _ (T_Pident _) }
L_Pint { PT _ (T_Pint _) }
L_Pbool { PT _ (T_Pbool _) }
L_Pstring { PT _ (T_Pstring _) }
L_Preal { PT _ (T_Preal _) }
L_Pchar { PT _ (T_Pchar _) }

%left ASS
%left EXP
%left EXPINLEXP
%left IDLEXP
%left CALL
%left 'not'
%left 'or' 'and'
%right '('
%right ')'
%right '{'
%nonassoc '==' '~=' '<' '<=' '>' '>=' '='
%left '+' '-'
%left '*' '/' '%'
%left '^' 
%right REF  '&'
%left NEG
%nonassoc '++' '--'
%nonassoc UNTIL RET
%left PARLEXP

%%

Integer :: {Integer} : L_integ  { (read ( $1)) :: Integer }
Pident    :: {Pident} : L_Pident { Pident (mkPosToken $1)}
Pint    :: {Pint} : L_Pint { Pint (mkPosToken $1)}
Pbool    :: {Pbool} : L_Pbool { Pbool (mkPosToken $1)}
Pstring    :: {Pstring} : L_Pstring { Pstring (mkPosToken $1)}
Preal    :: {Preal} : L_Preal { Preal (mkPosToken $1)}
Pchar    :: {Pchar} : L_Pchar { Pchar (mkPosToken $1)}

Program :: { Program }
Program : ListDec {Progr (reverse $1)}
ListDec :: { [Dec] }
ListDec : {- empty -} { [] } | ListDec Dec { flip (:) $1 $2 }
Dec :: { Dec }
Dec : 'function' Type_specifier Pident '('ListArgument')'ListDecStm 'end' { Func $2 $3 $5 (length $5)  (reverse $7) }
    |'function' Pident '('ListArgument')'ListDecStm 'end' { Func Tvoid $2 $4 (length $4) (reverse $6) }
    | Type_specifier Pident {VarDeclar $1 $2 Nothing}
    | Type_specifier Pident '=' Exp { VarDeclar $1 $2 (Just $4) }
Type_specifier :: { Type_specifier }
Type_specifier :'boolean' { Tbool }
          | 'character' { Tchar }
          | 'float' { Tfloat }
          | 'integer' { Tint }
          | 'string' { Tstring }
          | 'void'{ Tvoid }
          | 'pointer' Type_specifier { Tpointer $2 }
          | '{}' Type_specifier { Tarray Nothing $2 }
          --| '{'Exp'}' Type_specifier { Tarray (Just $2) $4 } 
Argument :: { Argument }
Argument : Modality Type_specifier Pident { FormPar $1 $2 $3 }
ListArgument :: { [Argument] }
ListArgument : {- empty -} { [] }
              | Argument { (:[]) $1 }
              | Argument ',' ListArgument { (:) $1 $3 }
Modality :: { Modality }
Modality : {- empty -} {  Modality_VAL }
         | 'val' { Modality_VAL }
         | 'res' { Modality_RES }
         | 'valres' { Modality_VALRES }
         | 'name' { Modality_NAME }
         | 'const' { Modality_CONST }
         | 'ref' { Modality_REF }
Stm :: { Stm }
Stm : Lexp Assignment_Op Exp %prec ASS {Assgn $2 $1 $3 }
    | 'return' Exp %prec RET { Valreturn $2 }
    |  Exp %prec EXP { SExp $1 }
    | 'if' Exp 'then' ListDecStm 'end' {SimpleIf $2 (reverse $4) }
    | 'if' Exp 'then' ListDecStm 'else' ListDecStm 'end' { IfThElse $2 (reverse $4) (reverse $6) }
    | 'while' Exp 'do' ListDecStm 'end' { While $2 (reverse $4) }
    | 'repeat' ListDecStm 'until' Exp %prec UNTIL { DoWhile $2 $4 } 

DecStm :: { DecStm }
DecStm : Dec { Dec $1 } | Stm { Stmt $1 }

ListDecStm :: { [DecStm] }
ListDecStm : {- empty -} { [] }
           | ListDecStm DecStm { flip (:) $1 $2 }

Exp :: { Exp }
Exp : Pident '(' ListExp ')' %prec CALL { Fcall $1 $3 (length $3) }
     |'('Exp')' {$2}
     |Exp 'or' Exp {InfixOp (BoolOp Or) $1 $3 }
     |Exp 'and' Exp { InfixOp (BoolOp And) $1 $3 }
     |Exp '==' Exp { InfixOp (RelOp Eq) $1 $3 }
     |Exp '~=' Exp { InfixOp (RelOp Neq) $1 $3 }
     |Exp '<' Exp { InfixOp (RelOp Lt) $1 $3 }
     |Exp '>' Exp { InfixOp (RelOp Gt) $1 $3 }
     |Exp '<=' Exp { InfixOp (RelOp LtE) $1 $3 }
     |Exp '>=' Exp { InfixOp (RelOp GtE) $1 $3 }
     |Exp '+' Exp { InfixOp (ArithOp Add) $1 $3 }
     |Exp '-' Exp { InfixOp (ArithOp Sub)$1 $3 }
     |Exp '*' Exp {InfixOp (ArithOp Mul) $1 $3 }
     |Exp '/' Exp {InfixOp (ArithOp Div) $1 $3 }
     |Exp '^' Exp {InfixOp (ArithOp Pow) $1 $3 }
     |Exp '%' Exp {InfixOp (ArithOp Mod)$1 $3 }
     |'&' Exp  { Addr $2 }
     | '-' Exp %prec NEG { Unary_Op Neg $2}
     | 'not' Exp { Unary_Op Logneg $2 }
     |'{' ListExp '}' { Arr $2 }
     | Lexp %prec EXPINLEXP { $1 }
     | Preal { Efloat $1 }
     | Pint  { Eint $1 }
     | Pbool { Ebool $1 }
     | Pstring {Estring $1 }
     | Pchar { Echar $1 }
     | '++' Lexp  {PrePost (Pre Incr) $2} 
     | '--'Lexp  {PrePost (Pre Decr) $2} 
     | Lexp '++'{PrePost (Post Incr) $1} 
     | Lexp '--'  {PrePost (Post Decr) $1} 

ListExp :: { [Exp] }
ListExp : {- empty -} { [] }
         | Exp { (:[]) $1 }
         | Exp ',' ListExp { (:) $1 $3 }

Lexp :: { Exp }
Lexp :  Pident %prec IDLEXP { Evar $1 } --%prec IDLEXP 
     | '('Lexp')' %prec PARLEXP {$2} 
     | '*' Exp %prec REF  { Indirection $2 } 
     | Exp '{' Exp '}' { Arraysel $1 $3 }
Assignment_Op :: { Assignment_Op }
Assignment_Op : '=' { Assign }
              | '*=' { AssgnArith Mul }
              | '/=' { AssgnArith Div }
              | '%=' { AssgnArith Mod }
              | '+=' { AssgnArith Add }
              | '-=' { AssgnArith Sub }
              | '^=' { AssgnArith Pow }
              | '&=' { AssgnBool And }
              | '|=' { AssgnBool Or}
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

