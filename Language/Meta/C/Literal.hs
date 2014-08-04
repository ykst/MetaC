module Language.Meta.C.Literal where
import qualified Language.Meta.SExpr as S

data Literal =
    LiteralInt IntegerConstant 
    | LiteralChar CharConstant
    | LiteralString StringLiteral
    | LiteralFloat FloatingConstant
    | LiteralEnumConst EnumerationConstant

data IntegerConstant =
    DecimalConstant String [IntegerSuffix]
    | OctalConstant String [IntegerSuffix]
    | HexadecimalConstant String [IntegerSuffix]

data FloatingConstant =
    DecimalFloatingConstant FractionalConstant (Maybe ExponentPart) (Maybe FloatingSuffix)
    | HexadecimalFloatingConstant HexadecimalFractionalConstant BinaryExponentPart (Maybe FloatingSuffix)

data FractionalConstant =
    FractionalConstant (Maybe String) String
    | MantissaConstant String

data HexadecimalFractionalConstant =
    HexadecimalFractionalConstant (Maybe String) String
    | HexadecimalMantissaConstant String

data ExponentPart =
    ExponentPart (Maybe FloatingConstantSign) String

data BinaryExponentPart =
    BinaryExponentPart (Maybe FloatingConstantSign) String

data FloatingConstantSign =
    FloatingConstantSignMinus
    | FloatingConstantSignPlus

data FloatingSuffix =
    FloatSuffix
    | LongDoubleSuffix

data IntegerSuffix =
    UnsignedSuffix
    | LongSuffix
    | LongLongSuffix

data CharConstant =
    CharConstant String
    | WideCharConstant String

data StringLiteral =
    StringLiteral S.SExpr
    | WideStringLiteral S.SExpr

data EnumerationConstant =
    EnumerationConstant String
