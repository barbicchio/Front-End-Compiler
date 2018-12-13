module SkelLua where

-- Haskell module generated by the BNF converter

import AbsLua
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transPbreak :: Pbreak -> Result
transPbreak x = case x of
  Pbreak string -> failure x
transPcontinue :: Pcontinue -> Result
transPcontinue x = case x of
  Pcontinue string -> failure x
transPbool :: Pbool -> Result
transPbool x = case x of
  Pbool string -> failure x
transPident :: Pident -> Result
transPident x = case x of
  Pident string -> failure x
transPint :: Pint -> Result
transPint x = case x of
  Pint string -> failure x
transPstring :: Pstring -> Result
transPstring x = case x of
  Pstring string -> failure x
transPreal :: Preal -> Result
transPreal x = case x of
  Preal string -> failure x
transPchar :: Pchar -> Result
transPchar x = case x of
  Pchar string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Progr decs -> failure x
transDec :: Dec -> Result
transDec x = case x of
  DecFunction functiondef -> failure x
  VarDeclar typespecifier pident -> failure x
  InitDeclarI typespecifier pident exp -> failure x
transType_specifier :: Type_specifier -> Result
transType_specifier x = case x of
  Typespec basictype -> failure x
  Typecompl complextype -> failure x
transBasicType :: BasicType -> Result
transBasicType x = case x of
  BasicType_boolean -> failure x
  BasicType_character -> failure x
  BasicType_float -> failure x
  BasicType_integer -> failure x
  BasicType_string -> failure x
  BasicType_void -> failure x
transComplexType :: ComplexType -> Result
transComplexType x = case x of
  Tarray typespecifier -> failure x
  Tpointer typespecifier -> failure x
transFunction_def :: Function_def -> Result
transFunction_def x = case x of
  Func typespecifier pident arguments chunkstm -> failure x
transArgument :: Argument -> Result
transArgument x = case x of
  ParamInt modality typespecifier pident -> failure x
transModality :: Modality -> Result
transModality x = case x of
  Modality1 -> failure x
  Modality_val -> failure x
  Modality_const -> failure x
  Modality_ref -> failure x
transStm :: Stm -> Result
transStm x = case x of
  CompStm chunkstm -> failure x
  ReturnStm returnstm -> failure x
  ExpStm exp -> failure x
  IterStm condstm -> failure x
transCond_stm :: Cond_stm -> Result
transCond_stm x = case x of
  TryCatch chunkstm1 chunkstm2 -> failure x
  If exp chunkstm -> failure x
  IfThenElse exp chunkstm1 chunkstm2 -> failure x
  While exp chunkstm -> failure x
  DoWhile chunkstm exp -> failure x
  For exp chunkstm -> failure x
transChunk_stm :: Chunk_stm -> Result
transChunk_stm x = case x of
  Chunkstm decs stms -> failure x
transReturn_stm :: Return_stm -> Result
transReturn_stm x = case x of
  ReturnStm1 exp -> failure x
transExp :: Exp -> Result
transExp x = case x of
  Fcall pident actpar -> failure x
  Eassign lexp assignmentop exp -> failure x
  Elor exp1 exp2 -> failure x
  Eland exp1 exp2 -> failure x
  Eeq exp1 exp2 -> failure x
  Eneq exp1 exp2 -> failure x
  Elthen exp1 exp2 -> failure x
  Egrthen exp1 exp2 -> failure x
  Elt exp1 exp2 -> failure x
  Egt exp1 exp2 -> failure x
  Eplus exp1 exp2 -> failure x
  Eminus exp1 exp2 -> failure x
  Etimes exp1 exp2 -> failure x
  Ediv exp1 exp2 -> failure x
  Emod exp1 exp2 -> failure x
  EPow exp1 exp2 -> failure x
  Epreop unaryoperator exp -> failure x
  Efloat preal -> failure x
  Einteger pint -> failure x
  Eboolean pbool -> failure x
  Estring pstring -> failure x
  Echar pchar -> failure x
  Lexp lexp -> failure x
  ArrayExp exps -> failure x
transActPar :: ActPar -> Result
transActPar x = case x of
  ActParam exps -> failure x
transLexp :: Lexp -> Result
transLexp x = case x of
  Evar pident -> failure x
  Indirection exp -> failure x
  Arraysel pident exp -> failure x
  InlineIf exp1 exp2 exp3 -> failure x
transUnary_operator :: Unary_operator -> Result
transUnary_operator x = case x of
  Address -> failure x
  Logicalneg -> failure x
  Negnum -> failure x
transAssignment_op :: Assignment_op -> Result
transAssignment_op x = case x of
  Assign -> failure x
  AssignMul -> failure x
  AssignDiv -> failure x
  AssignMod -> failure x
  AssignAdd -> failure x
  AssignSub -> failure x
  AssgnPow -> failure x
  AssgnAnd -> failure x
  AssgnOr -> failure x

