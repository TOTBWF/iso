{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import Data.Text (Text)
import Control.Monad (mapM)
import Data.List (find)
import qualified Data.Text as T

data Type
    = TVoid           -- Void Type, uninhabited
    | TUnit           -- Unit Type, inhabited by only ()
    | TBool           -- Primitive Bit Type, inhabited by true and false
    | TSum Type Type  -- Sum Type
    | TProd Type Type -- Product Type
    | TName Text      -- User defined type
    deriving (Show, Eq)

-- | Returns the number of inhabitants of a type

-- size ctx (TName n) = do 
--     (TypeDef _ cases) <- lookupType ctx n
--     return (sum $ (mapM (\t -> size ctx =<< lookupType ctx t)) $ cases)

-- data Value
--     = VUnit             -- Constructs an element of the unit type
--     | VBool Bool        -- Constructs an element of the primitive boolean type
--     | VName Text        -- Named Value
--     | VLeft Value       -- Constructs an the LHS of an anonymous sum type
--     | VRight Value      -- Constructs an the RHS of an anonymous sum type
--     | VProd Value Value -- Constructs a product type
--     deriving (Show)

data PatternBody
    = Empty
    | Const Text PatternBody
    | Bind Text PatternBody
    | App Text [Text] PatternBody
    | Nested Pattern PatternBody
    deriving (Show)

data Pattern = Pattern Text PatternBody
    deriving (Show)

data Iso 
    = Iso Text (Type, Type) [(Pattern, Pattern)]
    | Sym Iso
    | Compose Iso Iso
    deriving (Show)

type TypeDef = (Text, Type)

data Context = Context
    {
      isoDefs :: [Iso]
    , typeDefs :: [TypeDef]
    }

emptyCtx :: Context
emptyCtx = Context 
    {
      isoDefs = []
    , typeDefs = []
    }

lookupIso :: Context -> Text -> Maybe Iso
lookupIso ctx t = find (go) $ isoDefs ctx
    where 
    go (Iso t' _ _) = t' == t
    go _ = False

lookupType :: Context -> Text -> Maybe Type
lookupType ctx t = snd <$> find (go) (typeDefs ctx)
    where
    go (t', _) = t' == t

extendIso :: Context -> Iso -> Context
extendIso ctx i = Context 
    {
      isoDefs = i:isoDefs ctx
    , typeDefs = typeDefs ctx
    }

extendType :: Context -> TypeDef -> Context
extendType ctx t = Context 
    {
      isoDefs = isoDefs ctx
    , typeDefs = t:typeDefs ctx
    } 

