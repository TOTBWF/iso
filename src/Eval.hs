{-# LANGUAGE TupleSections #-}
module Eval where

import Syntax
import Context
import Data.Text (Text)
import Data.List (union, find)
import Data.Maybe (catMaybes)

-- | Evaluates a function Left to Right
evalLeft :: Context -> Iso -> Value -> Value
evalLeft ctx (_, _, ps) v = 
    (\(bs,p) -> bindPattern ctx bs p) $ head $ catMaybes $ (\(p1, p2) -> (,p2) <$> matchPattern ctx p1 v) <$> ps

matchPattern :: Context -> Pattern -> Value -> Maybe [(Text, Value)]
matchPattern _ (PUnit) (VUnit) = Just []
matchPattern _ (PBool b1) (VBool b2) | b1 == b2 = Just []
matchPattern ctx (PLeft p) (VLeft v) = matchPattern ctx p v
matchPattern ctx (PRight p) (VRight v) = matchPattern ctx p v
matchPattern _ (PBind n) v = Just [(n, v)]
matchPattern ctx (PApp f p') v = matchPattern ctx p' <$> flip (evalRight ctx) v =<< lookupIso ctx f
matchPattern ctx (PProd p1 p2) (VProd v1 v2) = union <$> matchPattern ctx p1 v1 <*> matchPattern ctx p2 v2
matchPattern _ _ _ = Nothing

bindPattern :: Context -> [(Text, Value)] -> Pattern -> Value
bindPattern _ _ (PUnit) = VUnit
bindPattern _ _ (PBool b) = VBool b
bindPattern ctx bs (PLeft p) = VLeft $ bindPattern ctx bs p
bindPattern ctx bs (PRight p) = VRight $ bindPattern ctx bs p
bindPattern _ bs (PBind n) = case find ((==) n . fst) bs of
    Just (_, v) -> v -- We know that this is true
bindPattern ctx bs (PProd p1 p2) = VProd (bindPattern ctx bs p1) (bindPattern ctx bs p2)
bindPattern ctx bs (PApp f p) = case lookupIso ctx f of
    Just i -> evalLeft ctx i (bindPattern ctx bs p) -- We know this case must be true

evalRight :: Context -> Iso -> Value -> Value
evalRight ctx i v = evalLeft ctx (invert i) v

invert :: Iso -> Iso
invert (n, t, ps) = (n, swap t, swap <$> ps)
    where
    swap :: (a,a) -> (a,a)
    swap (a,b) = (b,a)