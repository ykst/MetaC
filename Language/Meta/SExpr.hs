{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Language.Meta.SExpr where
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import qualified Language.Haskell.Meta.Parse as MP (parseExp)
import Data.Data
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.String (Parser)
import Control.Applicative hiding ((<|>), many, optionMaybe, optional)

data SExpr = SList [SItem] deriving (Typeable, Data)
data SItem = SString String | SCommandItem SCommand deriving (Typeable, Data)
data SCommand = SExp String | SVar deriving (Typeable, Data)

$(deriveLiftMany [
    ''SExpr,
    ''SItem,
    ''SCommand])

instance Show SExpr where
    show (SList items) = concatMap show items

instance Show SItem where
    show item = case item of
        SString s -> s
        SCommandItem command -> show command

instance Show SCommand where
    show scommand = case scommand of
        SExp s -> "${" ++ s ++ "}"
        SVar -> "${}"

sExprP :: Parser Char -> Parser SExpr
sExprP legalP = SList <$> many sItemP
    where
    sItemP :: Parser SItem
    sItemP =
        SString <$> many1 (legalP <|> try (char '$' <* notFollowedBy (char '{')))
        <|> SCommandItem <$> sCommandP

sCommandP :: Parser SCommand
sCommandP = string "${" >>
            (SVar <$ char '}'
            <|> SExp <$> (manyTill anyChar (try (string "}"))))

sExprStringP :: Parser SExpr
sExprStringP = sExprP (noneOf "$")

parseSExpr :: FilePath -> String -> SExpr
parseSExpr path = either (error . show) id . parse (sExprStringP <* eof) path

buildExp :: SExpr -> Q Exp
buildExp (SList items) =
    mapM toExpM items >>= pure . unzip >>= \(names, exps) ->
    lamE (map varP $ catMaybes names) (foldr appE [|""|] exps)

toExpM :: SItem -> Q (Maybe Name, Q Exp)
toExpM item = case item of
    SString str -> pure (Nothing, [| (++) str |])
    SCommandItem command_item -> toCommandM command_item >>= \(nm, exp) -> pure (nm, [| (++) $(exp) |])

toCommandM :: SCommand -> Q (Maybe Name, Q Exp)
toCommandM command = case command of
    SExp exp -> pure (Nothing, [| $(either (error. show) pure (MP.parseExp exp)) |])
    SVar -> newName "x" >>= \name -> pure (Just name, [| $(varE name) |])

showLoc :: String -> Loc -> String
showLoc ctx loc = intercalate ":" [ctx, loc_filename loc, show line, show col]
    where (line, col) = loc_start loc

s :: QuasiQuoter
s = QuasiQuoter {
    quoteExp = (\input -> location >>= \loc -> buildExp (parseSExpr (showLoc "s" loc) input)),
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}
