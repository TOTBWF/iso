{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Syntax where

import Data.Text (Text)
import qualified Data.Text as T

data Type
    = TVoid           -- Void Type, uninhabited
    | TUnit           -- Unit Type, inhabited by only ()
    | TBool           -- Primitive Bit Type, inhabited by true and false
    | TSum Type Type  -- Sum Type
    | TProd Type Type -- Product Type
    deriving (Show)

data Value
    = VUnit             -- Constructs an element of the unit type
    | VBool Bool        -- Constructs an element of the primitive boolean type
    | VName Text        -- Named Value
    | VLeft Value       -- Constructs an the LHS of an anonymous sum type
    | VRight Value      -- Constructs an the RHS of an anonymous sum type
    | VProd Value Value -- Constructs a product type
    deriving (Show)

data PatternBody
    = Empty
    | Const Text PatternBody
    | Bind Text PatternBody
    | App Text [Text] PatternBody
    | Nested Pattern PatternBody
    deriving (Show)

data Pattern = Pattern Text PatternBody
    deriving (Show)
data Iso = Iso Text (Type, Type) [(Pattern, Pattern)]
    deriving (Show)
--     = ZeroE | ZeroI 
--     | SwapPlus | AssocLPlus | AssocRPlus
--     | UnitE | UnitI
--     | SwapMul | AssocLMul | AssocRMul
--     | DistribZero | FactorZero
--     | Distrib | Factor
--     | Fold | Unfold
--     | Id | Sym Iso | Compose Iso Iso   
--     | SumI Iso Iso | ProdI Iso Iso
--     | Trace Iso
