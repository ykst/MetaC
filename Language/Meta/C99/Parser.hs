module Language.Meta.C99.Parser where
import Text.Parsec
import Text.Parsec.Combinator (manyTill)
import Debug.Trace
import Control.Applicative hiding ((<|>), many, optionMaybe, optional)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad (liftM, foldM)
import Control.Arrow (second)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Language.Meta.SExpr
import Language.Meta.C.AST
import Language.Meta.C.Literal
import Language.Meta.C.Parser
import Language.Meta.C99.AST
import Language.Meta.C99.Show

-- data TranslationUnit = NonEmptyList ExternalDecl
--     | WildExternalDecl SCommand
translationUnitP :: TheParser TranslationUnit
translationUnitP = TranslationUnit <$> nonEmptyListP externalDeclP

-- data ExternalDecl =
--     FunctionDefinition [DeclSpec] Declarator [Decl] Compound
--     | ExternalDecl Decl
--     | InternalTranslationUnit TranslationUnit
externalDeclP :: TheParser ExternalDecl
externalDeclP =
    InternalTranslationUnit <$> (WildTranslationUnit <$> lx sCommandP)
    <|> try (FunctionDefinition <$> many (notFollowedBy (try functionDeclaratorP) >> declSpecP) <*> functionDeclaratorP <*> many declNoInitP <*> compoundP)
    <|> try (ExternalDecl <$> declP)

-- data DeclSpec =
--     StorageDecl StorageClassSpec
--     | TypeDecl TypeSpec
--     | QualifierDecl TypeQualifier
--     | FunctionDecl FunctionSpec
declSpecP :: TheParser DeclSpec
declSpecP =
    StorageDecl <$> storageClassSpecP
    <|> try (TypeDecl <$> typeSpecP)
    <|> FunctionDecl <$> functionSpecP
    <|> QualifierDecl <$> typeQualifierP

-- data FunctionSpec =
--     InlineFunction
functionSpecP :: TheParser FunctionSpec
functionSpecP = InlineFunction <$ keyword "inline"

-- data Decl =
--     Decl (NonEmptyList DeclSpec) [InitDeclarator]
declP :: TheParser Decl
declP =
    try (Decl <$> nonEmptyListFollowedByP declSpecP (latterP >> pure ()) <*> latterP)
    <|> try (DeclNegligible <$> negligibleP <* blanks)
    where
    latterP = initDeclaratorListP <* semicolon

declNoInitP :: TheParser Decl
declNoInitP = Decl <$> nonEmptyListFollowedByP declSpecP (noInitDeclaratorListP >> semicolon >> pure ()) <*> (noInitDeclaratorListP <* semicolon)

-- data StorageClassSpec =
--     StorageAuto
--     | StorageRegister
--     | StorageStatic
--     | StorageExtern
--     | StorageTypedef
storageClassSpecP :: TheParser StorageClassSpec
storageClassSpecP = StorageAuto <$ keyword "auto"
    <|> StorageRegister <$ keyword "register"
    <|> StorageStatic <$ keyword "static"
    <|> StorageExtern <$ keyword "extern"
    <|> StorageTypedef <$ keyword "typedef"

-- data TypeSpec =
--     TypeVoid
--     | TypeChar
--     | TypeShort
--     | TypeInt
--     | TypeLong
--     | TypeFloat
--     | TypeDouble
--     | TypeSigned
--     | TypeUnsigned
--     | StructOrUnionCompoundSpec StructOrUnion (Maybe Id) (NonEmptyList StructDecl)
--     | StructOrUnionSpec StructOrUnion Id
--     | EnumCompundSpec (Maybe Id) (NonEmptyList Enumerator)
--     | EnumSpec Id
--     | TypeDef Id
typeSpecP :: TheParser TypeSpec
typeSpecP =
    TypeVoid <$ keyword "void"
    <|> TypeChar <$ keyword "char"
    <|> TypeShort <$ keyword "short"
    <|> TypeInt <$ keyword "int"
    <|> TypeLong <$ keyword "long"
    <|> TypeFloat <$ keyword "float"
    <|> TypeDouble <$ keyword "double"
    <|> TypeSigned <$ keyword "signed"
    <|> TypeUnsigned <$ keyword "unsigned"
    <|> TypeBool <$ keyword "_Bool"
    <|> TypeComplex <$ keyword "_Complex"
    <|> (structOrUnionP >>= \struct_or_union ->
         try (StructOrUnionCompoundSpec struct_or_union <$> optionMaybe idP <*> braces (nonEmptyListP structDeclP))
          <|> StructOrUnionSpec struct_or_union <$> idP)
    <|> (keyword "enum" >>
            (try (EnumCompundSpec <$> (optionMaybe idP) <*> braces (nonEmptyCommaListP enumeratorP))
            <|> EnumSpec <$> idP))
    <|> try (TypeDefName <$> idP)

-- data TypeQualifier =
--     ConstQualifier
--     | VolatileQualifier
--     | RestrictQualifier
typeQualifierP :: TheParser TypeQualifier
typeQualifierP =
    ConstQualifier <$ keyword "const"
    <|> VolatileQualifier <$ keyword "volatile"
    <|> RestrictQualifier <$ keyword "restrict"

-- data StructOrUnion =
--     Struct
--     | Union
structOrUnionP :: TheParser StructOrUnion
structOrUnionP = Struct <$ keyword "struct"
    <|> Union <$ keyword "union"

-- type InitDeclaratorList = [InitDeclarator]
initDeclaratorListP :: TheParser InitDeclaratorList
initDeclaratorListP = sepBy initDeclaratorP comma

noInitDeclaratorListP :: TheParser InitDeclaratorList
noInitDeclaratorListP = sepBy noInitDeclaratorP comma

-- data InitDeclarator =
--     InitDeclarator Declarator (Maybe Initializer)
initDeclaratorP :: TheParser InitDeclarator
initDeclaratorP = InitDeclarator <$> declaratorP <*> optionMaybe (equal >> initializerP)

noInitDeclaratorP :: TheParser InitDeclarator
noInitDeclaratorP = InitDeclarator <$> declaratorP <*> pure Nothing

-- data StructDecl =
--     StructDecl SpecQualifierList (NonEmptyList StructDeclarator)
structDeclP :: TheParser StructDecl
structDeclP = StructDecl <$> specQualifierListP' <*> (nonEmptyCommaListP structDeclaratorP <* semicolon)
    where
    specQualifierListP' :: TheParser SpecQualifierList
    specQualifierListP' =
        SpecQualifierList <$> nonEmptyListFollowedByP (eitherP typeSpecP typeQualifierP) (structDeclaratorP >> pure ())

-- data SpecQualifierList =
--     SpecQualifierList (NonEmptyList (Either TypeSpec TypeQualifier))
specQualifierListP :: TheParser SpecQualifierList
specQualifierListP =
    SpecQualifierList <$> nonEmptyListP (eitherP typeSpecP typeQualifierP)

-- data StructDeclarator =
--     StructDeclarator Declarator
--     | StructBitFieldDeclarator (Maybe Declarator) ConstExp
structDeclaratorP :: TheParser StructDeclarator
structDeclaratorP =
    try (StructBitFieldDeclarator <$> optionMaybe declaratorP <*> (colon >> constExpP))
    <|> StructDeclarator <$> declaratorP

-- data Enumerator =
--     Enumerator Id (Maybe ConstExp)
enumeratorP :: TheParser Enumerator
enumeratorP = Enumerator <$> enumerationConstantP <*> optionMaybe (equal >> constExpP)

-- data Declarator =
--     Declarator (Maybe Pointer) DirectDeclarator
declaratorP :: TheParser Declarator
declaratorP = Declarator <$> (optionMaybe pointerP) <*> directDeclaratorP

functionDeclaratorP :: TheParser Declarator
functionDeclaratorP = Declarator <$> (optionMaybe pointerP) <*> directFunctionDeclaratorP

-- data DirectDeclarator =
--     DirectDeclaratorId Id
--     | DirectDeclaratorParens Declarator
--     | DirectDeclaratorArray DirectDeclarator [TypeQualifier] (Maybe AssignmentExp)
--     | DirectDeclaratorStaticArray DirectDeclarator [TypeQualifier] AssignmentExp
--     | DirectDeclaratorPostStaticArray DirectDeclarator [TypeQualifier] AssignmentExp
--     | DirectDeclaratorVariableArray DirectDeclarator [TypeQualifier]
--     | DirectDeclaratorParamList DirectDeclarator ParamTypeList
--     | DirectDeclaratorIdList DirectDeclarator [Id]
directDeclaratorP :: TheParser DirectDeclarator
directDeclaratorP =
    (try (DirectDeclaratorParens <$> parens declaratorP)
     <|> try (DirectDeclaratorId <$> idP)) >>= directDeclaratorP_rec <?> "direct_declarator"
    where
    directDeclaratorP_rec :: DirectDeclarator -> TheParser DirectDeclarator
    directDeclaratorP_rec direct_declarator =
        ((DirectDeclaratorArray direct_declarator <$> arrayDeclaratorP
        <|> try (DirectDeclaratorParamList direct_declarator <$> parens paramTypeListP)
        <|> try (DirectDeclaratorIdList direct_declarator <$> parens (many idP))) >>= directDeclaratorP_rec)
        <|> pure direct_declarator

directFunctionDeclaratorP :: TheParser DirectDeclarator
directFunctionDeclaratorP =
     (DirectDeclaratorId <$> idP) >>= \id ->
     try (DirectDeclaratorParamList id <$> parens paramTypeListP)
     <|> try (DirectDeclaratorIdList id <$> parens (many idP))

-- data ArrayDeclarator =
--     ArrayDeclarator [TypeQualifier] (Maybe AssignmentExp)
--     | StaticArrayDeclarator [TypeQualifier] AssignmentExp
--     | PostStaticArrayDeclarator [TypeQualifier] AssignmentExp
--     | VariableArrayDeclarator [TypeQualifier]
arrayDeclaratorP :: TheParser ArrayDeclarator
arrayDeclaratorP =
        symbol "[" >> (
            {-ArrayDeclarator [] Nothing <$ symbol "]"
            <|> -} arrayDeclaratorP' <* symbol "]")
    where
    arrayDeclaratorP' :: TheParser ArrayDeclarator
    arrayDeclaratorP' =
        StaticArrayDeclarator <$> (keyword "static" >> many typeQualifierP) <*> assignmentExpP
             <|> (many typeQualifierP >>= \qualifiers ->
                  PostStaticArrayDeclarator qualifiers <$> (keyword "static" >> assignmentExpP)
                  <|> VariableArrayDeclarator qualifiers <$ keyword "*"
                  <|> ArrayDeclarator qualifiers <$> optionMaybe assignmentExpP)

-- data Pointer =
--     Pointer (NonEmptyList [TypeQualifier])
pointerP :: TheParser Pointer
pointerP = Pointer <$> (symbol "*" >> many typeQualifierP) <*> optionMaybe pointerP

-- data ParamTypeList =
--     ParamTypeList (NonEmptyList ParamDecl) (Maybe VaArg)
paramTypeListP :: TheParser ParamTypeList
paramTypeListP = ParamTypeList <$> nonEmptyCommaListP paramDeclP <*> optionMaybe (comma >> vaArgP)

-- data VaArg = VaArg
vaArgP :: TheParser VaArg
vaArgP = VaArg <$ symbol "..."

-- data ParamDecl =
--     ParamDecl (NonEmptyList DeclSpec) (Either Declarator (Maybe AbstractDeclarator))
paramDeclP :: TheParser ParamDecl
paramDeclP = ParamDecl <$> (nonEmptyListFollowedByP declSpecP (latterP >> pure ())) <*> latterP
    where
    latterP = eitherP declaratorP (optionMaybe abstractDeclaratorP)

-- data InitializerList =
--     InitializerList (NonEmptyList (Maybe Designation, Initializer))
initializerListP :: TheParser InitializerList
initializerListP =
    InitializerList <$> nonEmptyOptionalCommaListP ((,) <$> optionMaybe (designationP <* equal) <*> initializerP)

-- data Initializer =
--     InitializerAssignment Assignment
--     | InitializerCompound (NonEmptyList Initializer)
initializerP :: TheParser Initializer
initializerP =
    InitializerAssignment <$> assignmentExpP
    <|> InitializerCompound <$> braces initializerListP

-- data Designation =
--     Designation DesignatorList
designationP :: TheParser Designation
designationP = Designation <$> designatorListP

-- data DesignatorList =
--     DesignatorList Designator (Maybe DesignatorList)
designatorListP :: TheParser DesignatorList
designatorListP = DesignatorList <$> designatorP <*> optionMaybe designatorListP

-- data Designator =
--     DesignatorDot Id
--     | DesignatorArray ConstExp
designatorP :: TheParser Designator
designatorP =
    DesignatorDot <$> (symbol "." >> idP)
    <|> DesignatorArray <$> angles constExpP

-- data TypeName =
--     TypeName SpecQualifierList (Maybe AbstractDeclarator)
typeNameP :: TheParser TypeName
typeNameP = TypeName <$> specQualifierListP <*> optionMaybe abstractDeclaratorP

-- data AbstractDeclarator =
--     AbstractPointerDeclarator Pointer
--     | AbstractDeclarator (Maybe Pointer) DirectAbstractDeclarator
abstractDeclaratorP :: TheParser AbstractDeclarator
abstractDeclaratorP =
    AbstractPointerDeclarator <$> pointerP
    <|> AbstractDeclarator <$> (optionMaybe pointerP) <*> directAbstractDeclaratorP

-- data DirectAbstractDeclarator =
--     DirectAbstractDeclarator AbstractDeclarator [Either (Maybe ArrayDeclarator) (Maybe ParamTypeList)]
directAbstractDeclaratorP :: TheParser DirectAbstractDeclarator
directAbstractDeclaratorP =
    DirectAbstractDeclarator <$> optionMaybe (parens abstractDeclaratorP)
                             <*> many (eitherP arrayDeclaratorP
                                               (parens (optionMaybe paramTypeListP)))

-- data Stat =
--     Label Stat
--     | Case ConstExp Stat
--     | Default Stat
--     | ExpStat (Maybe Exp)
--     | CompoundStat Compound
--     | If Exp Stat (Maybe Stat)
--     | Switch Exp Stat
--     | While Exp Stat
--     | Do Stat Exp
--     | For (Maybe Exp) (Maybe Exp) (Maybe Exp) Stat
--     | Goto Id
--     | Continue
--     | Break
--     | Return (Maybe Exp)
statP :: TheParser Stat
statP =
    StatNegligible <$> negligibleP <* blanks
    <|> Case <$> (keyword "case" >> constExpP) <*> (colon >> statP)
    <|> Default <$> (keyword "default" >> colon >> statP)
    <|> CompoundStat <$> compoundP
    <|> If <$> (keyword "if"  >> parens expP) <*> statP <*> optionMaybe (keyword "else" >> statP)
    <|> Switch <$> (keyword "switch" >>  parens expP) <*> statP
    <|> While <$> (keyword "while" >> parens expP) <*> statP
    <|> Do <$> (keyword "do" >> statP) <*> (keyword "while" >> parens expP) <* semicolon
    <|> For <$> (keyword "for" >> symbol "(" >> optionMaybe expP) <*> (semicolon >> optionMaybe expP) <*> (semicolon >> optionMaybe expP <* symbol ")") <*> statP
    <|> Goto <$> (keyword "goto" >> idP) <* semicolon
    <|> Continue <$ keyword "continue" <* semicolon
    <|> Break <$ keyword "break" <* semicolon
    <|> Return <$> (keyword "return" >> optionMaybe expP) <* semicolon
    <|> try (ExpStat <$> optionMaybe expP <* semicolon)
    <|> try (Label <$> idP <*> (colon >> statP))

-- data Compound = Compound BlockItemList
compoundP :: TheParser Compound
compoundP = Compound <$> (symbol "{" >> blockItemListP <* symbol "}")

-- data BlockItemList =
--     BlockItemList [BlockItem]
--     | WildBlockItemList SCommand
blockItemListP :: TheParser BlockItemList
blockItemListP =
    BlockItemList <$> many blockItemP

-- data BlockItem =
--     BlockItem (Either Stat Decl)
--     | BlockItemInnerList BlockItemList
blockItemP :: TheParser BlockItem
blockItemP =
    BlockItemInnerList <$> (WildBlockItemList <$> lx sCommandP)
    <|> BlockItem <$> eitherP statP declP

-- data Exp = AssignmentExp (NonEmptyList Assignment)
expP :: TheParser Exp
expP = Exp <$> nonEmptyCommaListP assignmentExpP

-- data AssignmentExp =
--     AssignmentExp UnaryExp AssignmentOperator AssignmentExp
--     | AssignmentConditionalExp ConditionalExp
assignmentExpP :: TheParser AssignmentExp
assignmentExpP =
    try (AssignmentExp <$> unaryExpP <*> assignmentOperatorP <*> assignmentExpP)
    <|> AssignmentConditionalExp <$> conditionalExpP

-- data AssignmentOperator =
--     Assign
--     | AssignMult
--     | AssignDiv
--     | AssignMod
--     | AssignPlus
--     | AssignMinus
--     | AssignLShift
--     | AssignRShift
--     | AssignAnd
--     | AssignXor
--     | AssignOr
assignmentOperatorP :: TheParser AssignmentOperator
assignmentOperatorP =
    try (Assign <$ (equal >> notFollowedBy equal))
    <|> (choice $ map (try . uncurry (<$) . second symbol) [
            (AssignMult, "*="),
            (AssignDiv, "/="),
            (AssignMod, "%="),
            (AssignPlus, "+="),
            (AssignMinus, "-="),
            (AssignLShift, "<<="),
            (AssignRShift, ">>="),
            (AssignAnd, "&="),
            (AssignXor, "^="),
            (AssignOr, "|=")
        ])

-- data ConditionalExp =
--     ConditionalExp LogicalOrExp
--     | ConditionalTernaryExp LogicalOrExp Exp ConditionalExp
conditionalExpP :: TheParser ConditionalExp
conditionalExpP =
    logicalOrExpP >>= \logical_or_exp ->
    ConditionalTernaryExp logical_or_exp <$> ((symbol "?" >> expP) <* symbol ":") <*> conditionalExpP
    <|> pure (ConditionalExp logical_or_exp)

-- data ConstExp =
--     ConstExp ConditionalExp
constExpP :: TheParser ConstExp
constExpP = ConstExp <$> conditionalExpP

-- data LogicalOrExp =
--     LogicalOrExp CastExp [(BinaryOperator, CastExp)]
logicalOrExpP :: TheParser LogicalOrExp
logicalOrExpP = foldr logicalOrExpBinaryP logicalOrExpUnaryP binaryOperatorP

logicalOrExpBinaryP :: TheParser BinaryOperator -> TheParser LogicalOrExp -> TheParser LogicalOrExp
logicalOrExpBinaryP opP p = foldl (uncurry . LogicalOrExpBinary) <$> p <*> (many ((,) <$> opP <*> p))

logicalOrExpUnaryP :: TheParser LogicalOrExp
logicalOrExpUnaryP = LogicalOrExpUnary <$> castExpP

-- data BinaryOperator =
--     LogicalOr
--     | LogicalAnd
--     | BitOr
--     | BitXor
--     | BitAnd
--     | EqualityNotEqual
--     | EqualityEqual
--     | RelationalEGT
--     | RelationalELT
--     | RelationalGT
--     | RelationalLT
--     | ShiftRight
--     | ShiftLeft
--     | AdditivePlus
--     | AdditiveMinus
--     | MultMod
--     | MultDiv
--     | MultMult
binaryOperatorP :: [TheParser BinaryOperator]
binaryOperatorP = map (try . uncurry (<$)) [
        (LogicalOr, symbol "||"),
        (LogicalAnd, symbol "&&"),
        (BitOr, symbol "|" <* notFollowedBy (oneOf "|")),
        (BitXor, symbol "^"),
        (BitAnd, symbol "&" <* notFollowedBy (oneOf "&")),
        (EqualityEqual, symbol "=="),
        (EqualityNotEqual, symbol "!="),
        (RelationalEGT, symbol ">="),
        (RelationalELT, symbol "<="),
        (RelationalGT, symbol ">" <* notFollowedBy (oneOf "=")),
        (RelationalLT, symbol "<" <* notFollowedBy (oneOf "=")),
        (ShiftRight, symbol ">>"),
        (ShiftLeft, symbol "<<"),
        (BinaryPlus, symbol "+"),
        (BinaryMinus, symbol "-"),
        (Mod, symbol "%"),
        (Div, symbol "/"),
        (Mult, symbol "*")
    ]

-- data CastExp =
--     CastExp TypeName CastExp
--     | CastNone UnaryExp
--     | CompoundLiteral TypeName InitializerList
--     | WildExp SCommand
castExpP :: TheParser CastExp
castExpP =
    WildExp <$> lx sCommandP
    <|> try (parens typeNameP >>= \type_name ->
        CompoundLiteral type_name <$> braces initializerListP
        <|> CastExp type_name <$> castExpP)
    <|> CastNone <$> unaryExpP

-- data UnaryExp =
--     UnaryExp TerminalExp [PostExp]
--     | PreIncrement UnaryExp
--     | PreDecrement UnaryExp
--     | UnaryOp UnaryOperator CastExp
--     | SizeofExp UnaryExp
--     | SizeofType TypeName
unaryExpP :: TheParser UnaryExp
unaryExpP =
    PreIncrement <$> (symbol "++" >> unaryExpP)
    <|> PreDecrement <$> (symbol "--" >> unaryExpP)
    <|> UnaryOp <$> unaryOperatorP <*> castExpP
    <|> (keyword "sizeof" >> (try (SizeofType <$> parens typeNameP) <|>
                              SizeofExp <$> unaryExpP))
    <|> try (UnaryExp <$> terminalExpP <*> many postExpP)

-- data TerminalExp =
--     TerminalId Id
--     | TerminalLiteral Literal
--     | TerminalParens Exp
terminalExpP :: TheParser TerminalExp
terminalExpP =
    TerminalId <$> idP
    <|> TerminalLiteral <$> literalP
    <|> TerminalParens <$> parens expP

-- data PostExp =
--     PostArray Exp
--     | PostCall [Assignment]
--     | PostDot Id
--     | PostArrow Id
--     | PostIncrement
--     | PostDecrement
postExpP :: TheParser PostExp
postExpP =
    PostArray <$> angles expP
    <|> PostDot <$> (symbol "." >> idP)
    <|> try (PostCall <$> parens (sepBy assignmentExpP comma))
    <|> try (PostArrow <$> (symbol "->" >> idP))
    <|> try (PostIncrement <$ symbol "++")
    <|> try (PostDecrement <$ symbol "--")

-- data UnaryOperator =
--     UnaryReference
--     | UnaryDereference
--     | UnaryPlus
--     | UnaryMinus
--     | UnaryInverse
--     | UnaryNot
unaryOperatorP :: TheParser UnaryOperator
unaryOperatorP = choice $
    map (try . uncurry (<$) . second symbol) [
        (UnaryReference, "&"), (UnaryDereference, "*"), (UnaryPlus, "+"),
        (UnaryMinus, "-"), (UnaryInverse, "~"), (UnaryNot, "!")
    ]
-- -- interface
fromFile :: FilePath -> IO (Either String TranslationUnit)
fromFile path = readFile path >>= \src ->
    pure (either (error . show) (Right) (fromString path src))

fromString :: FilePath -> String -> Either ParseError TranslationUnit
fromString path = parse (blanks >> translationUnitP <* eof) path

expFromString :: FilePath -> String -> Either ParseError Exp
expFromString path = parse (blanks >> expP <* eof) path

statFromString :: FilePath -> String -> Either ParseError BlockItemList
statFromString path = parse (blanks >> blockItemListP <* eof) path
