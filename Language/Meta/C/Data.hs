{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Language.Meta.C.Data where
import Data.Data
import Language.Meta.C.Literal
import Language.Meta.C.AST

deriving instance Typeable Id
deriving instance Typeable Negligible
deriving instance Typeable StorageClassSpec
deriving instance Typeable VaArg
deriving instance Typeable StructOrUnion
deriving instance Typeable UnaryOperator
deriving instance Typeable BinaryOperator
deriving instance Typeable AssignmentOperator
deriving instance Typeable Literal
deriving instance Typeable IntegerConstant
deriving instance Typeable FloatingConstant
deriving instance Typeable FractionalConstant
deriving instance Typeable HexadecimalFractionalConstant
deriving instance Typeable ExponentPart
deriving instance Typeable BinaryExponentPart
deriving instance Typeable FloatingConstantSign
deriving instance Typeable FloatingSuffix
deriving instance Typeable IntegerSuffix
deriving instance Typeable CharConstant
deriving instance Typeable StringLiteral
deriving instance Typeable EnumerationConstant

deriving instance Data Id
deriving instance Data Negligible
deriving instance Data StorageClassSpec
deriving instance Data VaArg
deriving instance Data StructOrUnion
deriving instance Data UnaryOperator
deriving instance Data BinaryOperator
deriving instance Data AssignmentOperator
deriving instance Data Literal
deriving instance Data IntegerConstant
deriving instance Data FloatingConstant
deriving instance Data FractionalConstant
deriving instance Data HexadecimalFractionalConstant
deriving instance Data ExponentPart
deriving instance Data BinaryExponentPart
deriving instance Data FloatingConstantSign
deriving instance Data FloatingSuffix
deriving instance Data IntegerSuffix
deriving instance Data CharConstant
deriving instance Data StringLiteral
deriving instance Data EnumerationConstant
