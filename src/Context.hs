module Context where

import Syntax
import Data.Text (Text)
import Data.List (find)

data Context = Context
    {
      isoDefs :: [Iso]
    , typeDefs :: [TypeDef]
    }
    deriving (Show)

emptyCtx :: Context
emptyCtx = Context 
    {
      isoDefs = []
    , typeDefs = []
    }

lookupIso :: Context -> Text -> Maybe Iso
lookupIso ctx t = find (go) $ isoDefs ctx
    where 
    go (t', _, _) = t' == t
    -- go _ = False

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
