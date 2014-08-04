{-# LANGUAGE DeriveDataTypeable #-}
module Language.Meta.C99.AST where
import Language.Meta.SExpr
import Language.Meta.C.AST
import Language.Meta.C.Literal

-- ISO/IEC 9899:TC3

-- translation_unit	: external_decl+
data TranslationUnit =
    TranslationUnit (NonEmptyList ExternalDecl)
    | WildTranslationUnit SCommand

-- external_decl		: decl_spec* declarator decl* compound_stat
-- 				/* function definition */
-- 			| decl
data ExternalDecl =
    FunctionDefinition [DeclSpec] Declarator [Decl] Compound
    | ExternalDecl Decl
    | InternalTranslationUnit TranslationUnit

-- decl_spec		: storage_class_spec | type_spec | type_qualifier | function_spec
data DeclSpec =
    StorageDecl StorageClassSpec
    | TypeDecl TypeSpec
    | QualifierDecl TypeQualifier
    | FunctionDecl FunctionSpec

-- function_spec : 'inline'
data FunctionSpec =
    InlineFunction

-- decl			: decl_spec+ init_declarator_list? ';'
data Decl =
    Decl (NonEmptyList DeclSpec) InitDeclaratorList
    | DeclNegligible Negligible

-- type_spec		: 'void' | 'char' | 'short' | 'int' | 'long' | 'float'
-- 			| 'double' | 'signed' | 'unsigned' | _Bool | _Complex
-- 			| struct_or_union id? '{' struct_decl+ '}'
-- 			| struct_or_union id
-- 			| 'enum' id? '{' enumerator ( ',' enumerator )* '}'
-- 			| 'enum' id
-- 			| typedef_name
--
-- typedef_name		: id
data TypeSpec =
    TypeVoid
    | TypeChar
    | TypeShort
    | TypeInt
    | TypeLong
    | TypeFloat
    | TypeDouble
    | TypeSigned
    | TypeUnsigned
    | TypeBool
    | TypeComplex
    | StructOrUnionCompoundSpec StructOrUnion (Maybe Id) (NonEmptyList StructDecl)
    | StructOrUnionSpec StructOrUnion Id
    | EnumCompundSpec (Maybe Id) (NonEmptyList Enumerator)
    | EnumSpec Id
    | TypeDefName Id

-- type_qualifier		: 'const' | 'volatile' | 'restrict'
data TypeQualifier =
    ConstQualifier
    | VolatileQualifier
    | RestrictQualifier

-- init_declarator_list	: init_declarator ( ',' init_declarator )*
type InitDeclaratorList = [InitDeclarator]

-- init_declarator		: declarator
-- 			| declarator '=' initializer
data InitDeclarator =
    InitDeclarator Declarator (Maybe Initializer)

-- struct_decl		: spec_qualifier_list struct_declarator
-- 				( ',' struct_declarator )* ';'
data StructDecl =
    StructDecl SpecQualifierList (NonEmptyList StructDeclarator)

-- spec_qualifier_list	: ( type_spec | type_qualifier )+
data SpecQualifierList =
    SpecQualifierList (NonEmptyList (Either TypeSpec TypeQualifier))

-- struct_declarator	: declarator
-- 			| declarator? ':' const_exp
data StructDeclarator =
    StructDeclarator Declarator
    | StructBitFieldDeclarator (Maybe Declarator) ConstExp

-- enumerator		: id
-- 			| id '=' const_exp
data Enumerator =
    Enumerator EnumerationConstant (Maybe ConstExp)

-- declarator		: pointer? direct_declarator
data Declarator =
    Declarator (Maybe Pointer) DirectDeclarator

-- direct_declarator	: id
-- 			| '(' declarator ')'
-- 			| direct_declarator '[' type_qualifier* assignment_exp? ']'
-- 			| direct_declarator '[' 'static' type_qualifier* assignment_exp ']'
-- 			| direct_declarator '[' type_qualifier* 'static' assignment_exp ']'
-- 			| direct_declarator '[' type_qualifier* '*' ']'
-- 			| direct_declarator '(' param_type_list ')'
-- 			| direct_declarator '(' id_list? ')'
--
-- id_list			: id ( ',' id )*
data DirectDeclarator =
    DirectDeclaratorId Id
    | DirectDeclaratorParens Declarator
    | DirectDeclaratorArray DirectDeclarator ArrayDeclarator
    | DirectDeclaratorParamList DirectDeclarator ParamTypeList
    | DirectDeclaratorIdList DirectDeclarator [Id]

data ArrayDeclarator =
    ArrayDeclarator [TypeQualifier] (Maybe AssignmentExp)
    | StaticArrayDeclarator [TypeQualifier] AssignmentExp
    | PostStaticArrayDeclarator [TypeQualifier] AssignmentExp
    | VariableArrayDeclarator [TypeQualifier]

-- pointer			: ( '*' type_qualifier* )+
data Pointer =
    Pointer [TypeQualifier] (Maybe Pointer)

-- param_type_list		: param_decl ( ',' param_decl )* ( ',' '...' | )
data ParamTypeList =
    ParamTypeList (NonEmptyList ParamDecl) (Maybe VaArg)

-- param_decl		: decl_spec+ ( declarator | abstract_declarator? )
data ParamDecl =
    ParamDecl (NonEmptyList DeclSpec) (Either Declarator (Maybe AbstractDeclarator))

-- initializer		: designation? assignment_exp
-- 			| '{' initializer ( ',' initializer )* ( ',' | ) '}'
data InitializerList =
    InitializerList (NonEmptyList (Maybe Designation, Initializer))

data Initializer =
    InitializerAssignment AssignmentExp
    | InitializerCompound InitializerList

-- designation      : ( '[' const_exp ']'
--          | '.' id )+ '='
data Designation =
    Designation DesignatorList

data DesignatorList =
    DesignatorList Designator (Maybe DesignatorList)

data Designator =
    DesignatorDot Id
    | DesignatorArray ConstExp

-- type_name		: spec_qualifier_list abstract_declarator?
data TypeName =
    TypeName SpecQualifierList (Maybe AbstractDeclarator)

-- abstract_declarator	: pointer
-- 			| pointer? direct_abstract_declarator
data AbstractDeclarator =
    AbstractPointerDeclarator Pointer
    | AbstractDeclarator (Maybe Pointer) DirectAbstractDeclarator

-- direct_abstract_declarator: '(' abstract_declarator ')'
-- 				( '[' type_qualifier* assignment_exp? ']'
-- 				| '[' 'static' type_qualifier* assignment_exp ']'
-- 				| '[' type_qualifier* 'static' assignment_exp ']'
-- 				| '[' * ']'
-- 				| '(' param_type_list? ')' )*
data DirectAbstractDeclarator =
    DirectAbstractDeclarator (Maybe AbstractDeclarator) [Either ArrayDeclarator (Maybe ParamTypeList)]

-- stat			: id ':' stat
-- 			| 'case' const_exp ':' stat
-- 			| 'default' ':' stat
-- 			| exp? ';'
-- 			| compound_stat
-- 			| 'if' '(' exp ')' stat
-- 			| 'if' '(' exp ')' stat 'else' stat
-- 			| 'switch' '(' exp ')' stat
-- 			| 'while' '(' exp ')' stat
-- 			| 'do' stat 'while' '(' exp ')' ';'
-- 			| 'for' '(' exp? ';' exp? ';' exp? ')' stat
-- 			| 'goto' id ';'
-- 			| 'continue' ';'
-- 			| 'break' ';'
-- 			| 'return' exp? ';'
data Stat =
    Label Id Stat
    | Case ConstExp Stat
    | Default Stat
    | ExpStat (Maybe Exp)
    | CompoundStat Compound
    | If Exp Stat (Maybe Stat)
    | Switch Exp Stat
    | While Exp Stat
    | Do Stat Exp
    | For (Maybe Exp) (Maybe Exp) (Maybe Exp) Stat
    | Goto Id
    | Continue
    | Break
    | Return (Maybe Exp)
    | StatNegligible Negligible

-- compound_stat		: '{' ( decl | stat )* '}'
data Compound = Compound BlockItemList

data BlockItemList =
    BlockItemList [BlockItem]
    | WildBlockItemList SCommand

data BlockItem =
    BlockItem (Either Stat Decl)
    | BlockItemInnerList BlockItemList

-- exp			: assignment_exp ( ',' assignment_exp )*
data Exp = Exp (NonEmptyList AssignmentExp)

-- assignment_exp		: ( unary_exp assignment_operator )* conditional_exp
data AssignmentExp =
    AssignmentExp UnaryExp AssignmentOperator AssignmentExp
    | AssignmentConditionalExp ConditionalExp

-- conditional_exp		: ( logical_or_exp '?' exp ':' )* logical_or_exp
data ConditionalExp =
    ConditionalExp LogicalOrExp
    | ConditionalTernaryExp LogicalOrExp Exp ConditionalExp

-- const_exp		: conditional_exp
data ConstExp =
    ConstExp ConditionalExp

-- logical_or_exp		: cast_exp
-- 				( ( '*' | '/' | '%' | '+' | '-' | '<<' | '>>'
-- 				  | '<' | '>' | '<=' | '>=' | '==' | '!='
-- 				  | '&' | '^' | '|' | '&&' | '||' )
-- 				 cast_exp )*
data LogicalOrExp =
    LogicalOrExpUnary CastExp
    | LogicalOrExpBinary LogicalOrExp BinaryOperator LogicalOrExp

-- cast_exp		: ( '(' type_name ')' )* unary_exp
-- 			| '(' type_name ')' '{' initializer ( ',' initializer )* ( ',' | ) '}'
data CastExp =
    CastExp TypeName CastExp
    | CastNone UnaryExp
    | CompoundLiteral TypeName InitializerList
    | WildExp SCommand

-- unary_exp		: ( id | int_const | char_const | float_const
-- 				| enumeration_const | string | '(' exp ')'
-- 			  )
-- 			  (	'[' exp ']'
-- 				| '(' argument_exp_list? ')'
-- 				| '.' id
-- 				| '->' id
-- 				| '++' | '--'			%left!
-- 			  )*
-- 			| '++' unary_exp
-- 			| '--' unary_exp
-- 			| unary_operator cast_exp
-- 			| 'sizeof' unary_exp
-- 			| 'sizeof' '(' type_name ')'
--
-- argument_exp_list	: assignment_exp ( ',' assignment_exp )*
data UnaryExp =
    UnaryExp TerminalExp [PostExp]
    | PreIncrement UnaryExp
    | PreDecrement UnaryExp
    | UnaryOp UnaryOperator CastExp
    | SizeofExp UnaryExp
    | SizeofType TypeName

data TerminalExp =
    TerminalId Id
    | TerminalLiteral Literal
    | TerminalParens Exp

data PostExp =
    PostArray Exp
    | PostCall [AssignmentExp]
    | PostDot Id
    | PostArrow Id
    | PostIncrement
    | PostDecrement
