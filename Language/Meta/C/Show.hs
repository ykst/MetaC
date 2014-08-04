{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module Language.Meta.C.Show where
import Language.Meta.C.Literal
import Language.Meta.C.AST
import Language.Meta.SExpr
import Data.List (intercalate)

parens :: String -> String
parens x = "(" ++ x ++ ")"

angles :: String -> String
angles x = "[" ++ x ++ "]"

braces :: String -> String
braces x = "{" ++ x ++ "}"

spaces :: String -> String
spaces x = " " ++ x ++ " "

-- comma separated list
csl :: [String] -> String
csl xs = intercalate ", " xs

-- newline separated list
nsl, nnsl :: [String] -> String
nsl xs = intercalate eol xs
nnsl xs = intercalate (eol ++ eol) xs

eol :: String
eol = "\n"

indent :: Int -> String
indent 0 = ""
indent 1 = "    "
indent level = indent 1 ++ indent (level - 1)

flatten :: NonEmptyList a -> [a]
flatten (NonEmptyList (x, xs)) = (x:xs)

class ShowC a where
    showC :: Int -> a -> String

instance (ShowC a) => (Show a) where
    show x = showC 0 x

instance Show Negligible where
    show negligible = case negligible of
        NegligibleMacro macro -> macro
        NegligibleComment comment -> comment

instance Show Id where
    show (Id str) = str

instance Show Literal where
    show literal = case literal of
        LiteralInt integer -> show integer
        LiteralChar char -> show char
        LiteralFloat float -> show float
        LiteralString string -> show string
        LiteralEnumConst enum_const -> show enum_const

instance Show EnumerationConstant where
    show (EnumerationConstant str) = str

instance Show CharConstant where
    show char = case char of
        CharConstant s -> "'" ++ s ++ "'"
        WideCharConstant s -> "L'" ++ s ++ "'"

instance Show StringLiteral where
    show string = case string of
        StringLiteral s -> "\"" ++ show s ++ "\""
        WideStringLiteral s -> "L\"" ++ show s ++ "\""

instance Show IntegerConstant where
    show integer_constant = case integer_constant of
        DecimalConstant s suffixes -> s ++ concatMap show suffixes
        OctalConstant s suffixes -> "0" ++ s ++ concatMap show suffixes
        HexadecimalConstant s suffixes -> "0x" ++ s ++ concatMap show suffixes

instance Show IntegerSuffix where
    show suffix = case suffix of
        UnsignedSuffix -> "U"
        LongSuffix -> "L"
        LongLongSuffix -> "LL"

instance Show FloatingConstant where
    show float = case float of
        DecimalFloatingConstant fractional_constant exponent_part suffix -> show fractional_constant ++ maybe "" show exponent_part ++ maybe "" show suffix
        HexadecimalFloatingConstant fractional_constant exponent_part suffix -> "0x" ++ show fractional_constant ++ show exponent_part ++ maybe "" show suffix

instance Show FractionalConstant where
    show fractional = case fractional of
        FractionalConstant mantissa floating ->
            maybe "" id mantissa ++ "." ++ floating
        MantissaConstant mantissa ->
            mantissa ++ "."

-- TODO: DRY
instance Show HexadecimalFractionalConstant where
    show fractional = case fractional of
        HexadecimalFractionalConstant mantissa floating ->
            maybe "" id mantissa ++ "." ++ floating
        HexadecimalMantissaConstant mantissa ->
            mantissa ++ "."

instance Show ExponentPart where
    show exponent_part = case exponent_part of
        ExponentPart sign exponent ->
            "e" ++ maybe "" show sign ++ exponent

-- DRY
instance Show BinaryExponentPart where
    show exponent_part = case exponent_part of
        BinaryExponentPart sign exponent ->
            "p" ++ maybe "" show sign ++ exponent

instance Show FloatingConstantSign where
    show sign = case sign of
        FloatingConstantSignPlus -> "+"
        FloatingConstantSignMinus -> "-"

instance Show FloatingSuffix where
    show suffix = case suffix of
        FloatSuffix -> "f"
        LongDoubleSuffix -> "l"

instance Show StructOrUnion where
    show struct_or_union = case struct_or_union of
        Struct -> "struct"
        Union -> "union"

instance Show AssignmentOperator where
    show assignment_operator = case assignment_operator of
        Assign -> "="
        AssignMult -> "*="
        AssignDiv -> "/="
        AssignMod -> "%="
        AssignPlus -> "+="
        AssignMinus -> "-="
        AssignLShift -> "<<="
        AssignRShift -> ">>="
        AssignAnd -> "&="
        AssignXor -> "^="
        AssignOr -> "|="

instance Show BinaryOperator where
    show binary_operator = case binary_operator of
        LogicalOr -> "||"
        LogicalAnd -> "&&"
        BitOr -> "|"
        BitXor -> "^"
        BitAnd -> "&"
        EqualityNotEqual -> "!="
        EqualityEqual -> "=="
        RelationalEGT -> ">="
        RelationalELT -> "<="
        RelationalGT -> ">"
        RelationalLT -> "<"
        ShiftRight -> ">>"
        ShiftLeft -> "<<"
        BinaryPlus -> "+"
        BinaryMinus -> "-"
        Mod -> "%"
        Div -> "/"
        Mult -> "*"

instance Show UnaryOperator where
    show unary_operator = case unary_operator of
        UnaryReference -> "&"
        UnaryDereference -> "*"
        UnaryPlus -> "+"
        UnaryMinus -> "-"
        UnaryInverse -> "~"
        UnaryNot -> "!"
