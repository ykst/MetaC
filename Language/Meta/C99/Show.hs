{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module Language.Meta.C99.Show (
    showC
) where
import Language.Meta.C.AST
import Language.Meta.C.Show
import Language.Meta.C99.AST
import Language.Meta.C99.Data
import Language.Meta.C.Literal
import Data.Generics

instance ShowC TranslationUnit where
    showC level (TranslationUnit external_decls) =
        nnsl (map (nsl . map (showC level)) (split [] [] (flatten external_decls)))
        where
        split rs as (n1:n2:ns) = case n1 of
            ExternalDecl (DeclNegligible neg) -> case neg of
                (NegligibleComment _) -> matched
                _ -> case (n1, n2) of
                    (ExternalDecl (DeclNegligible (NegligibleMacro _)), ExternalDecl (DeclNegligible (NegligibleMacro _))) -> matched
                    _ -> unmatched
            _ -> unmatched
            where
            unmatched = split ((n1:as):rs) [] (n2:ns)
            matched = split rs (n1:as) (n2:ns)
        split rs as (n:ns) = split ((n:as):rs) [] ns
        split rs _ [] = reverse (map reverse rs)

instance ShowC ExternalDecl where
    showC level external_decl = (case external_decl of
        FunctionDefinition decl_specs declarator decls compound ->
            (unwords $ (map (showC level) decl_specs) ++ [show declarator]) ++ "\n" ++
            (case decls of
                [] -> ""
                _ -> (nsl $ map (showC level) decls) ++ "\n") ++ showC level compound
        ExternalDecl decl -> indent' ++ showC level decl
        InternalTranslationUnit translation_unit -> showC level translation_unit)
        where
        indent' = indent level

instance ShowC DeclSpec where
    showC level decls_spec = case decls_spec of
        StorageDecl storage_class_spec -> show storage_class_spec
        TypeDecl type_spec -> showC level type_spec
        QualifierDecl type_qualifier -> show type_qualifier
        FunctionDecl function_spec -> show function_spec

instance Show FunctionSpec where
    show spec = case spec of
        InlineFunction -> "inline"

instance ShowC Decl where
    showC level (Decl decl_specs init_declarator_list) =
        unwords (map (showC level) (flatten decl_specs) ++ map show init_declarator_list) ++ ";"
    showC level (DeclNegligible neg) = show neg

instance Show StorageClassSpec where
    show storage_class_spec = case storage_class_spec of
        StorageAuto -> "auto"
        StorageRegister -> "register"
        StorageStatic -> "static"
        StorageExtern -> "extern"
        StorageTypedef -> "typedef"

instance ShowC TypeSpec where
    showC level type_spec = case type_spec of
        TypeVoid -> "void"
        TypeChar -> "char"
        TypeShort -> "short"
        TypeInt -> "int"
        TypeLong -> "long"
        TypeFloat -> "float"
        TypeDouble -> "double"
        TypeSigned -> "signed"
        TypeUnsigned -> "unsigned"
        TypeBool -> "_Bool"
        TypeComplex -> "_Complex"
        StructOrUnionCompoundSpec struct_or_union id struct_decls ->
            nsl [show struct_or_union ++ maybe " " (spaces . show) id ++ "{",
                nsl $ map (showC (level + 1)) (flatten struct_decls),
                indent' ++ "}"]
        StructOrUnionSpec struct_or_union id ->
            unwords [show struct_or_union, show id]
        EnumCompundSpec id enumerators ->
            nsl ["enum" ++ maybe " " (spaces . show) id ++ "{",
                nsl $ map show (flatten enumerators),
                indent' ++ "}"]
        EnumSpec id -> unwords ["enum", show id]
        TypeDefName id -> show id
        where
        indent' = indent level

instance Show TypeQualifier where
    show type_qualifier = case type_qualifier of
        ConstQualifier -> "const"
        VolatileQualifier -> "volatile"
        RestrictQualifier -> "restrict"

instance Show InitDeclarator where
    show (InitDeclarator declarator initializer) = case initializer of
        Nothing -> show declarator
        Just initializer' -> unwords [show declarator, "=", show initializer']

instance ShowC StructDecl where
    showC level (StructDecl spec_qualifier_list struct_declarators) =
        indent' ++ unwords [showC level spec_qualifier_list,
                            unwords $ map show (flatten struct_declarators)] ++ ";"
        where
        indent' = indent level

instance ShowC SpecQualifierList where
    showC level (SpecQualifierList types_or_qualifiers) =
        unwords $ map (either (showC level) show) (flatten types_or_qualifiers)

instance Show StructDeclarator where
    show struct_declarator = case struct_declarator of
        StructDeclarator declarator ->
            show declarator
        StructBitFieldDeclarator declarator const_exp -> case declarator of
            Nothing -> ":" ++ show const_exp
            Just declarator' -> unwords [show declarator', ":"] ++ show const_exp

instance Show Enumerator where
    show (Enumerator id const_exp) = case const_exp of
        Nothing -> show id
        Just const_exp' -> unwords [show id, "=", show const_exp]

instance Show Declarator where
    show (Declarator pointer direct_declarator) = case pointer of
        Nothing -> show direct_declarator
        Just pointer' -> unwords [show pointer' ++ show direct_declarator]

instance Show DirectDeclarator where
    show direct_declarator = case direct_declarator of
        DirectDeclaratorId id -> show id
        DirectDeclaratorParens declarator -> parens (show declarator)
        DirectDeclaratorArray direct_declarator' array ->
            show direct_declarator' ++ show array
        DirectDeclaratorParamList direct_declarator' param_type_list ->
            show direct_declarator' ++ parens (show param_type_list)
        DirectDeclaratorIdList direct_declarator' ids ->
            show direct_declarator' ++ parens (unwords (map show ids))

instance Show ArrayDeclarator where
    show array = "[" ++ (case array of
        ArrayDeclarator qualifiers assignment_exp ->
            unwords (map show qualifiers ++ maybe [] (return . show) assignment_exp)
        StaticArrayDeclarator qualifiers assignment_exp ->
            unwords ("static":(map show qualifiers ++ [show assignment_exp]))
        PostStaticArrayDeclarator qualifiers assignment_exp ->
            unwords (map show qualifiers ++ ["static"] ++ [show assignment_exp])
        VariableArrayDeclarator qualifiers ->
            unwords (map show qualifiers ++ ["*"])) ++ "]"

instance Show Pointer where
    show (Pointer type_qualifiers pointer) = case type_qualifiers of
        [] -> "*" ++  maybe "" show pointer
        _ -> "* " ++  unwords (map show type_qualifiers) ++ " " ++ maybe "" show pointer

instance Show ParamTypeList where
    show (ParamTypeList param_decls vaarg) =
        csl (map show (flatten param_decls) ++ maybe [] (return . show) vaarg)

instance Show VaArg where
    show _ = "..."

instance Show ParamDecl where
    show (ParamDecl decl_specs declarator_or_abstruct_declarator) =
        unwords (map show (flatten decl_specs) ++ either (return . show) (maybe [] (return . show)) declarator_or_abstruct_declarator)

-- TODO: make it ShowC to deserialize long compound list
instance Show InitializerList where
    show (InitializerList list) = csl $ map showElem (flatten list)
        where
        showElem (Nothing, initializer) = show initializer
        showElem (Just designation, initializer) =
            unwords [show designation, "=", show initializer]

instance Show Initializer where
    show initializer = case initializer of
        InitializerAssignment assignment_exp -> show assignment_exp
        InitializerCompound initializers ->
            braces (show initializers)

instance Show Designation where
    show (Designation designator_list) = show designator_list

instance Show DesignatorList where
    show (DesignatorList designator designator_list) = case designator of
        DesignatorDot id -> "." ++ show id
        DesignatorArray const_exp -> angles (show const_exp) ++ maybe "" show designator_list

instance ShowC TypeName where
    showC level (TypeName spec_qualifier_list abstract_declarator) = case abstract_declarator of
        Nothing -> showC level spec_qualifier_list
        Just abstract_declarator' ->
            let qualifier_str = showC level spec_qualifier_list in
            case show abstract_declarator' of
                "" -> qualifier_str
                declarator_str -> unwords [qualifier_str, declarator_str]

instance Show AbstractDeclarator where
    show abstract_declarator = case abstract_declarator of
        AbstractPointerDeclarator pointer -> show pointer
        AbstractDeclarator pointer direct_abstract_declarator ->
            maybe "" show pointer ++ show direct_abstract_declarator

instance Show DirectAbstractDeclarator where
    show (DirectAbstractDeclarator abstract_declarator const_exp_or_param_type_list) =
        maybe "" (parens . show) abstract_declarator ++ concatMap (either show (parens . maybe "" show)) const_exp_or_param_type_list


instance ShowC Stat where
    showC level stat = case stat of
        Label id stat' -> unwords [show id, ":", showC level stat']
        Case const_exp stat' -> unwords ["case", show const_exp, ":", showC level stat']
        Default stat' -> unwords ["default", ":", showC level stat']
        ExpStat exp -> maybe "" show exp ++ ";"
        CompoundStat compound -> showC level compound
        If exp stat' else_stat -> unwords ["if", parens (show exp), showC level stat'] ++ maybe "" ((++) " else " . (showC level)) else_stat
        Switch exp stat' -> unwords ["switch", parens (show exp), showC level stat']
        While exp stat' -> unwords ["while", parens (show exp), showC level stat']
        Do stat' exp -> unwords ["do", showC level stat', "while", parens (show exp)] ++ ";"
        For exp1 exp2 exp3 stat' ->
            unwords ["for",
                     parens (maybe "" show exp1 ++ ";" ++
                             maybe "" ((++) " ". show) exp2 ++ ";" ++
                             maybe "" ((++) " ". show) exp3),
                     showC level stat']
        Goto id -> unwords ["goto", show id] ++ ";"
        Continue -> "continue;"
        Break -> "break;"
        Return exp -> case exp of
            Nothing -> "return;"
            Just exp' -> unwords ["return", show exp'] ++ ";"
        StatNegligible neg -> show neg

instance ShowC Compound where
    showC level (Compound block_item_list) = let block_item_list_str = showC (level + 1) block_item_list in
        case block_item_list_str of
            "" -> nsl ["{", indent (level + 1) ++ "/* empty block */", indent level ++ "}"]
            _ -> nsl ["{", block_item_list_str, (indent level) ++ "}"]

instance ShowC BlockItemList where
    showC level (WildBlockItemList scommand) = indent level ++ show scommand
    showC level (BlockItemList block_items) =
        nnsl $ map (nsl . map (showC level)) (split [] [] block_items)
        where
        split rs as (n1:n2:ns)
            | toConstr n1 /= toConstr n2 = unmatched
            | otherwise = case (n1, n2) of
                (BlockItem l, BlockItem r)
                    {- -- TODO: pretty formatting of comments and macros..
                    | eitherConstr l == toConstr (DeclNegligible undefined) -> matched
                    | eitherConstr l == toConstr (StatNegligible undefined) -> matched
                    | eitherConstr r == toConstr (StatNegligible undefined) -> matched
                    -}
                    | toConstr l /= toConstr r -> unmatched
                    | eitherConstr l /= eitherConstr r -> unmatched
                    | eitherConstr l == toConstr (ExpStat undefined) -> matched
                    | eitherConstr l == toConstr (Decl undefined undefined) -> matched
                    | otherwise -> unmatched
                _ -> unmatched
            where
            unmatched = split ((n1:as):rs) [] (n2:ns)
            matched = split rs (n1:as) (n2:ns)
            eitherConstr = either toConstr toConstr
        split rs as (n:ns) = split ((n:as):rs) [] ns
        split rs _ [] = reverse (map reverse rs)

instance ShowC BlockItem where
    showC level item = case item of
        BlockItem start_or_decl -> either showStat showDecl start_or_decl
        BlockItemInnerList list -> showC level list
        where
        showDecl :: Decl -> String
        showDecl (DeclNegligible neg@(NegligibleMacro _)) = show neg
        showDecl decl = indent level ++ showC level decl
        showStat :: Stat -> String
        showStat (StatNegligible neg@(NegligibleMacro _)) = show neg
        showStat stat = indent level ++  showC level stat

instance Show Exp where
    show (Exp assignment_exps) = csl $ map show (flatten assignment_exps)

instance Show AssignmentExp where
    show assignment_exp = case assignment_exp of
        AssignmentExp unary_exp assignment_operator assignment_exp' ->
            unwords [show unary_exp, show assignment_operator, show assignment_exp']
        AssignmentConditionalExp conditional_exp ->
            show conditional_exp

instance Show ConditionalExp where
    show conditional_exp = case conditional_exp of
        ConditionalExp logical_or_exp -> show logical_or_exp
        ConditionalTernaryExp logical_or_exp exp conditional_exp' ->
            unwords [show logical_or_exp, "?", show exp, ":", show conditional_exp']

instance Show ConstExp where
    show (ConstExp conditional_exp) = show conditional_exp

instance Show LogicalOrExp where
    show logical_or_exp = case logical_or_exp of
        LogicalOrExpUnary cast_exp -> show cast_exp
        LogicalOrExpBinary logical_or_exp_lhs binary_operator logical_or_exp_rhs ->
            unwords [show logical_or_exp_lhs, show binary_operator, show logical_or_exp_rhs]

instance Show CastExp where
    show cast_exp = case cast_exp of
        CastExp type_name cast_exp ->
            parens (show type_name) ++ show cast_exp
        CastNone unary_exp -> show unary_exp
        CompoundLiteral type_name initializer_list -> parens (show type_name) ++ braces (show initializer_list)

instance Show UnaryExp where
    show unary_exp = case unary_exp of
        UnaryExp terminal_exp post_exps ->
            show terminal_exp ++ concatMap show post_exps
        PreIncrement unary_exp ->
            "++" ++ show unary_exp
        PreDecrement unary_exp ->
            "--" ++ show unary_exp
        UnaryOp unary_operator cast_exp ->
            show unary_operator ++ show cast_exp
        SizeofExp unary_exp ->
            unwords ["sizeof", show unary_exp]
        SizeofType type_name ->
            "sizeof" ++ parens (show type_name)

instance Show TerminalExp where
    show terminal_exp = case terminal_exp of
        TerminalId id -> show id
        TerminalLiteral literal -> show literal
        TerminalParens exp -> parens (show exp)

instance Show PostExp where
    show post_exp = case post_exp of
        PostArray exp -> angles (show exp)
        PostCall assignment_exps -> parens (csl (map show assignment_exps))
        PostDot id -> "." ++ show id
        PostArrow id -> "->" ++ show id
        PostIncrement -> "++"
        PostDecrement -> "--"
