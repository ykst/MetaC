{-# LANGUAGE DeriveDataTypeable #-}
module Language.Meta.C.AST where
import Language.Meta.SExpr
import Data.Data

newtype NonEmptyList a = NonEmptyList (a, [a]) deriving (Typeable, Data)

-- %token int_const char_const float_const id string enumeration_const
data Id = Id String
    | WildId SExpr

data Negligible =
    NegligibleMacro String
    | NegligibleComment String

-- storage_class_spec	: 'auto' | 'register' | 'static' | 'extern' | 'typedef'
data StorageClassSpec =
    StorageAuto
    | StorageRegister
    | StorageStatic
    | StorageExtern
    | StorageTypedef

data VaArg = VaArg

-- struct_or_union		: 'struct' | 'union'
data StructOrUnion =
    Struct
    | Union

-- unary_operator		: '&' | '*' | '+' | '-' | '~' | '!'
data UnaryOperator =
    UnaryReference
    | UnaryDereference
    | UnaryPlus
    | UnaryMinus
    | UnaryInverse
    | UnaryNot

-- %left '*' '/' '%'
-- %left '+' '-'
-- %left '<<' '>>'
-- %left '<' '>' '<=' '>='
-- %left '==' '!='
-- %left '&'
-- %left '^'
-- %left '|'
-- %left '&&'
-- %left '||'
--
data BinaryOperator =
    LogicalOr
    | LogicalAnd
    | BitOr
    | BitXor
    | BitAnd
    | EqualityNotEqual
    | EqualityEqual
    | RelationalEGT
    | RelationalELT
    | RelationalGT
    | RelationalLT
    | ShiftRight
    | ShiftLeft
    | BinaryPlus
    | BinaryMinus
    | Mod
    | Div
    | Mult

-- assignment_operator	: '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<='
-- 			| '>>=' | '&=' | '^=' | '|='
data AssignmentOperator =
    Assign
    | AssignMult
    | AssignDiv
    | AssignMod
    | AssignPlus
    | AssignMinus
    | AssignLShift
    | AssignRShift
    | AssignAnd
    | AssignXor
    | AssignOr
