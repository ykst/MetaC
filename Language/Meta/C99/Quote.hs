{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables #-}
module Language.Meta.C99.Quote (
    c,
    ce,
    cs
) where
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax
import qualified Language.Haskell.Meta.Parse as MH (parseExp)
import qualified Language.Meta.C.Quote as CT
import qualified Language.Meta.C.AST as CA
import qualified Language.Meta.C.Literal as CL
import qualified Language.Meta.C.Parser as CP
import qualified Language.Meta.C99.AST as C99A
import Language.Meta.C99.Data
import Language.Meta.C99.Parser
import qualified Language.Meta.SExpr as S
import Data.Maybe (catMaybes)
import Data.Data
import Data.Generics
import Control.Monad (liftM)
import qualified Control.Monad.Trans as MT
import Control.Monad.Trans.State
import Text.Parsec (ParseError)

$(deriveLiftMany [
    ''C99A.TranslationUnit,
    ''C99A.ExternalDecl,
    ''C99A.DeclSpec,
    ''C99A.FunctionSpec,
    ''C99A.Decl,
    ''C99A.TypeSpec,
    ''C99A.TypeQualifier,
    ''C99A.InitDeclarator,
    ''C99A.StructDecl,
    ''C99A.SpecQualifierList,
    ''C99A.StructDeclarator,
    ''C99A.Enumerator,
    ''C99A.Declarator,
    ''C99A.DirectDeclarator,
    ''C99A.ArrayDeclarator,
    ''C99A.Pointer,
    ''C99A.ParamTypeList,
    ''C99A.ParamDecl,
    ''C99A.InitializerList,
    ''C99A.Initializer,
    ''C99A.Designation,
    ''C99A.DesignatorList,
    ''C99A.Designator,
    ''C99A.TypeName,
    ''C99A.AbstractDeclarator,
    ''C99A.DirectAbstractDeclarator,
    ''C99A.Stat,
    ''C99A.Compound,
    ''C99A.BlockItemList,
    ''C99A.BlockItem,
    ''C99A.Exp,
    ''C99A.AssignmentExp,
    ''C99A.ConditionalExp,
    ''C99A.ConstExp,
    ''C99A.LogicalOrExp,
    ''C99A.CastExp,
    ''C99A.UnaryExp,
    ''C99A.TerminalExp,
    ''C99A.PostExp])

c :: QuasiQuoter
c = QuasiQuoter {
    quoteExp = strToExp fromString,
    quotePat = strToPat fromString,
    quoteType = undefined,
    quoteDec = undefined
}

ce :: QuasiQuoter
ce = QuasiQuoter {
    quoteExp = strToExp expFromString,
    quotePat = strToPat expFromString,
    quoteType = undefined,
    quoteDec = undefined
}

cs :: QuasiQuoter
cs = QuasiQuoter {
    quoteExp = strToExp statFromString,
    quotePat = strToPat statFromString,
    quoteType = undefined,
    quoteDec = undefined
}

type QState = StateT [Name] Q

strToExp :: forall a. Data a => (FilePath -> String -> Either ParseError a) -> String -> Q Exp
strToExp p input = case p "c99expExpr" input of
    Left err -> error $ show err
    Right result -> do
        (exp, names) <- rs result
        lamE (map varP (reverse names)) exp

rs :: forall a. Data a => a -> Q (Q Exp, [Name])
rs result = runStateT (dataToExpQM ((\_ -> return Nothing)
    `extQ` antiId -- TODO: hey, what the hell is this?
    `extQ` antiExp
    `extQ` antiBlockItemList
    `extQ` antiTranslationUnit
    `extQ` antiStringLiteral) result) []

antiId (CA.WildId (S.SList items)) =
    MT.lift (mapM S.toExpM items) >>= return . unzip >>= \(names, exps) ->
    modify (reverse (catMaybes names) ++) >>
    MT.lift location >>= \loc ->
    return (Just [| CT.quoteId "antiId" loc ($(foldr appE [|""|] exps)) |])
antiId _ = return Nothing

antiBlockItemList (C99A.WildBlockItemList command) =
    MT.lift (S.toCommandM command) >>= \(name, exp) ->
    maybe (return ()) (modify . (:)) name >>
    MT.lift location >>= \loc ->
    return (Just [| quoteBlockItems "antiBlockItemList" loc ($(exp)) |])
antiBlockItemList _ = return Nothing

-- DRY
antiExp (C99A.WildExp command) =
    MT.lift (S.toCommandM command) >>= \(name, exp) ->
    maybe (return ()) (modify . (:)) name >>
    MT.lift location >>= \loc ->
    return (Just [| quoteCExp "antiExp" loc ($(exp)) |])
antiExp _ = return Nothing

antiTranslationUnit (C99A.WildTranslationUnit command) =
    MT.lift (S.toCommandM command) >>= \(name, exp) ->
    maybe (return ()) (modify . (:)) name >>
    MT.lift location >>= \loc ->
    return (Just [| quoteTranslationUnit "antiTranslationUnit" loc ($(exp)) |])
antiTranslationUnit _ = return Nothing

antiStringLiteral (CL.StringLiteral (S.SList items)) =
    MT.lift (mapM S.toExpM items) >>= return . unzip >>= \(names, exps) ->
    modify (reverse (catMaybes names) ++) >>
    return (Just [| CL.StringLiteral (S.SList [S.SString ($(foldr appE [|""|] exps))]) |])
antiStringLiteral (CL.WideStringLiteral (S.SList items)) =
    MT.lift (mapM S.toExpM items) >>= return . unzip >>= \(names, exps) ->
    modify (reverse (catMaybes names) ++) >>
    return (Just [| CL.WideStringLiteral (S.SList [S.SString ($(foldr appE [|""|] exps))]) |])

quoteBlockItems ctx = CT.quoteParser ctx blockItemListP
quoteCExp ctx = CT.quoteParser ctx castExpP 
quoteTranslationUnit ctx = CT.quoteParser ctx translationUnitP

strToPat p input = case p "c99Pat" input of
    Left err -> error $ show err
    Right result -> dataToPatQ (const Nothing) result

-- TODO: Refactor out dataToXxxM utilities
dataToQaM  ::  forall a k q m. (Data a, Monad m)
          =>  (Name -> k)
          ->  (Lit -> Q q)
          ->  (k -> [Q q] -> Q q)
          ->  (forall b. Data b => b -> m (Maybe (Q q)))
          ->  a
          ->  m (Q q)
dataToQaM mkCon mkLit appCon antiQ t =
    antiQ t >>= \antiResult ->
    case antiResult of
      Nothing ->
          case constrRep constr of
            AlgConstr _ ->
                conArgs >>= \args ->
                return (appCon (mkCon conName) args)
              where
                conName :: Name
                conName =
                    case showConstr constr of
                      "(:)"       -> Name (mkOccName ":") (NameG DataName (mkPkgName "ghc-prim") (mkModName "GHC.Types"))
                      con@"[]"    -> Name (mkOccName con) (NameG DataName (mkPkgName "ghc-prim") (mkModName "GHC.Types"))
                      con@('(':_) -> Name (mkOccName con) (NameG DataName (mkPkgName "ghc-prim") (mkModName "GHC.Tuple"))
                      con         -> mkNameG_d (tyConPackage tycon)
                                               (tyConModule tycon)
                                               con
                  where
                    tycon :: TyCon
                    tycon = (typeRepTyCon . typeOf) t

                conArgs :: m [Q q]
                conArgs = sequence (gmapQ (dataToQaM mkCon mkLit appCon antiQ) t)
            IntConstr n ->
                return $ mkLit $ integerL n
            FloatConstr n ->
                return $ mkLit $ rationalL n
            CharConstr c ->
                return $ mkLit $ charL c
        where
          constr :: Constr
          constr = toConstr t

      Just y -> return y

dataToExpQM :: (Data a, Monad m)
    => (forall b. Data b => b -> m (Maybe (Q Exp)))
    -> a
    -> m (Q Exp)
dataToExpQM = dataToQaM conE litE (foldl appE)
