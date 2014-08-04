module Language.Meta.C.Parser {- (
    TheParser, -- TODO: make it generic
    blank, blanks, lx, symbol, parens, braces, angles, keyword,
    eitherP, comma, colon, semicolon, equal, -- TODO: move them to more common module
    nonEmptyListP,  -- TODO: amend this ugly utility
    nonEmptyListFollowedByP,
    nonEmptyCommaListP,
    nonEmptyOptionalCommaListP,
    negligibleP,
    idP, literalP, enumerationConstantP
) -} where
import Text.Parsec
import Text.Parsec.Combinator (manyTill)
import Control.Applicative hiding ((<|>), many, optionMaybe, optional)
import Control.Monad.Identity (Identity)
import qualified Language.Meta.SExpr as S
import Language.Meta.C.Literal
import Language.Meta.C.AST
import Language.Meta.SExpr
import qualified Data.Set as Set

-- the type of parser
type TheParser =  ParsecT String () Identity

blank :: TheParser Char
blank = space -- oneOf " 　\t\r\n"

blanks :: TheParser ()
blanks= skipMany blank >> pure ()

lx :: TheParser a -> TheParser a
lx p = p <* blanks

symbol :: String -> TheParser String
symbol = try . lx . string

parens :: TheParser a -> TheParser a
parens = between (symbol "(") (symbol ")")

braces :: TheParser a -> TheParser a
braces = between (symbol "{") (symbol "}")

angles :: TheParser a -> TheParser a
angles = between (symbol "[") (symbol "]")

keyword :: String -> TheParser String
keyword s = try (lx $ string s <* notFollowedBy (alphaNum <|> char '_'))

-- type NonEmptyList a = (a, [a])
nonEmptyListP :: TheParser a -> TheParser (NonEmptyList a)
nonEmptyListP p = NonEmptyList <$> ((,) <$> (try p) <*> many (try p))

nonEmptyListFollowedByP :: TheParser a -> TheParser () -> TheParser (NonEmptyList a)
nonEmptyListFollowedByP ap bp = NonEmptyList <$> ((,) <$> try ap <*> many ((notFollowedBy (try bp)) >> try ap))

nonEmptyCommaListP :: TheParser a -> TheParser (NonEmptyList a)
nonEmptyCommaListP p =
    NonEmptyList <$> ((,) <$> (try p) <*> many (try $ comma >> p))

nonEmptyOptionalCommaListP :: TheParser a -> TheParser (NonEmptyList a)
nonEmptyOptionalCommaListP p = sepEndBy1 (try p) comma >>= \(car:cdr) ->
    pure (NonEmptyList (car, cdr))

eitherP :: TheParser a -> TheParser b -> TheParser (Either a b)
eitherP a b =
    Left <$> try a
    <|> Right <$> try b

comma :: TheParser ()
comma = symbol "," >> pure ()

colon :: TheParser ()
colon = symbol ":" >> pure ()

semicolon :: TheParser ()
semicolon = symbol ";" >> pure ()

equal :: TheParser ()
equal = symbol "=" >> pure ()

reservedSet :: Set.Set String
reservedSet = Set.fromList [
    "auto", "break", "case", "char", "const", "continue", "default",
    "do", "double", "else", "enum", "extern", "float", "for", "goto",
    "if", "inline", "int", "long", "register", "restrict", "return",
    "short", "signed", "sizeof", "static", "struct", "switch", "typedef",
    "union", "unsigned", "void", "volatile", "while", "_Bool", "_Complex",
    "_Imaginary"]

-- cheap subset of CPP
negligibleP :: TheParser Negligible
negligibleP =
    NegligibleMacro <$> ((++) <$> string "#" <*> many (noneOf "\r\n"))
    <|> NegligibleComment <$> try (string "/" >>
        ((++) "//" <$> (string "/" >> many (noneOf "\r\n"))
         <|> (\s -> "/*" ++ s ++ "*/") <$> (string "*" >> manyTill anyChar (try (string "*/")))))

-- data Id = Id String
idP :: TheParser Id
idP = lx (try (lookAhead carSExprP >> (sExprP cdrP <?> "sExpr")) >>= \sexpr -> case sexpr of
        SList [(SString s)] -> Id <$> checkReserved s
        _ -> pure (WildId sexpr))
    where 
    checkReserved ('$':_) = parserZero
    checkReserved s = if Set.member s reservedSet then parserZero else pure s
    carSExprP = oneOf (letters ++ "_$")
    --carP = oneOf (letters ++ "_")
    cdrP = oneOf (letters ++ ['0'..'9'] ++ "_")
    letters = ['a'..'z'] ++ ['A'..'Z']


-- data Literal =
--     LiteralInt IntegerConstant
--     | LiteralChar CharConstant
--     | LiteralString String
--     | LiteralFloat FloatingConstant
--     | LiteralEnumConst String
literalP :: TheParser Literal
literalP = lx . choice $ map try [
        LiteralString <$> stringLiteralP,
        LiteralChar <$> charConstantP,
        LiteralFloat <$> floatingConstantP,
        LiteralInt <$> integerConstantP,
        LiteralEnumConst <$> enumerationConstantP
    ]

enumerationConstantP :: TheParser EnumerationConstant
enumerationConstantP = idP >>= \(Id str) -> pure (EnumerationConstant str)

charConstantP :: TheParser CharConstant
charConstantP =
    WideCharConstant <$> (string "L'" >> charSequenceP quote)
    <|> CharConstant <$> (char quote >> charSequenceP quote)
    where quote = '\''

stringLiteralP :: TheParser StringLiteral
stringLiteralP =
    WideStringLiteral <$> ((foldl (++) "" <$> many1 (lx $ string "L\"" >> charSequenceP quote)) >>= pure . S.parseSExpr "wide string literal") -- TODO: is the concat right?
    <|> StringLiteral <$> ((foldl (++) "" <$> many1 (lx $ char quote >> charSequenceP quote)) >>= pure . S.parseSExpr "string literal")
    where quote = '"'

charSequenceP :: Char -> TheParser String
charSequenceP quote = charSequenceP' False []
    where
    charSequenceP' :: Bool -> String -> TheParser String
    charSequenceP' escaping str
        | escaping = anyChar >>= \c -> charSequenceP' False (c:str)
        | otherwise = anyChar >>= \c -> case c of
            x
                | x == quote -> pure (reverse str)
                | otherwise -> charSequenceP' (c == '\\') (c:str)

integerConstantP :: TheParser IntegerConstant
integerConstantP =
    DecimalConstant <$> decimalConstantP <*> integerSuffixP
    <|> (string "0" >>
        (((oneOf "xX" >> (HexadecimalConstant <$> hexadecimalConstantP <*> integerSuffixP))
            <|> OctalConstant <$> octalConstantP <*> integerSuffixP)
        <|> DecimalConstant "0" <$> integerSuffixP))
    where
    decimalConstantP = (:) <$> natDigit <*> many digit
    hexadecimalConstantP = many1 hexDigit
    octalConstantP = many1 octDigit

integerSuffixP :: TheParser [IntegerSuffix]
integerSuffixP = choice [
        glue <$> unsignedSuffixP <*> optionMaybe longlongSuffixP,
        glue <$> unsignedSuffixP <*> optionMaybe longSuffixP,
        glue <$> longlongSuffixP <*> optionMaybe unsignedSuffixP,
        glue <$> longSuffixP <*> optionMaybe unsignedSuffixP
    ] <|> pure []
    where
    glue a b = a:(maybe [] pure b)
    longlongSuffixP = try (LongLongSuffix <$ (string "ll" <|> string "LL"))
    longSuffixP = LongSuffix <$ oneOf "lL"
    unsignedSuffixP = UnsignedSuffix <$ oneOf "uU"

floatingConstantP :: TheParser FloatingConstant
floatingConstantP =
    try (string "0" >> oneOf "xX" >> (HexadecimalFloatingConstant <$> hexadecimalFractionalConstantP <*> binaryExponentP <*> optionMaybe floatingSuffixP))
    <|> DecimalFloatingConstant <$> fractionalConstantP <*> optionMaybe exponentPartP <*> optionMaybe floatingSuffixP

fractionalConstantP :: TheParser FractionalConstant
fractionalConstantP =
    many digit >>= \mantissa ->
    string "." >>
    many digit >>= \floating ->
    case floating of
        [] -> case mantissa of
            [] -> parserZero
            _ -> pure (MantissaConstant mantissa)
        _ -> case mantissa of
            [] -> pure (FractionalConstant Nothing floating)
            _ -> pure (FractionalConstant (Just mantissa) floating)

hexadecimalFractionalConstantP :: TheParser HexadecimalFractionalConstant
hexadecimalFractionalConstantP =
    many digit >>= \mantissa ->
    string "." >>
    many digit >>= \floating ->
    case floating of
        [] -> case mantissa of
            [] -> parserZero
            _ -> pure (HexadecimalMantissaConstant mantissa)
        _ -> case mantissa of
            [] -> pure (HexadecimalFractionalConstant Nothing floating)
            _ -> pure (HexadecimalFractionalConstant (Just mantissa) floating)

exponentPartP :: TheParser ExponentPart
exponentPartP =
    ExponentPart <$> (oneOf "eE" >> optionMaybe floatingConstantSignP) <*> many1 digit

binaryExponentP :: TheParser BinaryExponentPart
binaryExponentP =
    BinaryExponentPart <$> (oneOf "pP" >> optionMaybe floatingConstantSignP) <*> many1 digit

floatingConstantSignP :: TheParser FloatingConstantSign
floatingConstantSignP =
    FloatingConstantSignPlus <$ string "+"
    <|>FloatingConstantSignMinus <$ string "-"

floatingSuffixP :: TheParser FloatingSuffix
floatingSuffixP =
    FloatSuffix <$ oneOf "fF"
    <|> LongDoubleSuffix <$ oneOf "lL"

natDigit :: TheParser Char
natDigit = oneOf ['1'..'9']
