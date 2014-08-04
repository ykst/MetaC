{-# LANGUAGE TemplateHaskell #-}
module Language.Meta.C.Quote where
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import qualified Language.Meta.C.AST as AST
import qualified Language.Meta.C.Literal as L 
import Language.Meta.C.Parser
import Language.Meta.SExpr
import Data.Data
import Text.Parsec
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.String (Parser)
import Control.Applicative hiding ((<|>), many, optionMaybe, optional)

$(deriveLiftMany [
    ''Loc,
    ''AST.NonEmptyList,
    ''AST.Id,
    ''AST.Negligible,
    ''AST.StorageClassSpec,
    ''AST.VaArg,
    ''AST.StructOrUnion,
    ''AST.UnaryOperator,
    ''AST.BinaryOperator,
    ''AST.AssignmentOperator,
    ''L.Literal,
    ''L.IntegerConstant,
    ''L.FloatingConstant,
    ''L.FractionalConstant,
    ''L.HexadecimalFractionalConstant,
    ''L.ExponentPart,
    ''L.BinaryExponentPart,
    ''L.FloatingConstantSign,
    ''L.FloatingSuffix,
    ''L.IntegerSuffix,
    ''L.CharConstant,
    ''L.StringLiteral,
    ''L.EnumerationConstant])

quoteId :: String -> Loc -> String -> AST.Id
quoteId ctx = quoteParser ctx idP

quoteParser :: String -> TheParser a -> Loc -> String -> a
quoteParser ctx p loc = 
    either (error . show) id  . parse (blanks >> p <* eof) (showLoc ctx loc)
