{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import Data.Text (Text)
import Control.Monad (mapM)
import qualified Data.Text as T

data Type
    = TVoid           -- Void Type, uninhabited
    | TUnit           -- Unit Type, inhabited by only ()
    | TBool           -- Primitive Bit Type, inhabited by true and false
    | TSum Type Type  -- Sum Type
    | TProd Type Type -- Product Type
    | TName Text      -- User defined type
    deriving (Show, Eq)

data Value
    = VUnit             -- Constructs an element of the unit type
    | VBool Bool        -- Constructs an element of the primitive boolean type
    | VLeft Value       -- Constructs an the LHS of an anonymous sum type
    | VRight Value      -- Constructs an the RHS of an anonymous sum type
    | VProd Value Value -- Constructs a product type
    deriving (Show, Eq)

data Pattern
    = PUnit
    | PBool Bool
    | PLeft Pattern
    | PRight Pattern
    | PBind Text
    | PApp Text Pattern
    | PProd Pattern Pattern
    deriving (Show)

type Iso = (Text, (Type, Type), [(Pattern, Pattern)])

type TypeDef = (Text, Type)

data Decl 
    = TypeDecl TypeDef
    | IsoDecl Iso