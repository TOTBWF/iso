{-# LANGUAGE TupleSections #-}
module Eval(bindPattern) where

import Syntax
import Context
import Data.Text (Text)
import Data.List (union, find)
import Data.Maybe (catMaybes)

-- | Evaluates a function from Left to Right
eval :: Context -> Iso -> Value -> Value
eval ctx (_, _, ps) v = 
    (\(bs,p) -> bindPattern False ctx bs p) $ head $ catMaybes $ (\(p1, p2) -> (,p2) <$> matchPattern ctx p1 v) <$> ps

matchPattern :: Context -> Pattern -> Value -> Maybe [(Text, Value)]
matchPattern _ (PUnit) (VUnit) = Just []
matchPattern _ (PBool b1) (VBool b2) | b1 == b2 = Just []
matchPattern ctx (PLeft p) (VLeft v) = matchPattern ctx p v
matchPattern ctx (PRight p) (VRight v) = matchPattern ctx p v
matchPattern _ (PBind n) v = Just [(n, v)]
matchPattern ctx (PApp f p') v = matchPattern ctx p' <$> flip (eval ctx) v . invert =<< lookupIso ctx f
matchPattern ctx (PProd p1 p2) (VProd v1 v2) = union <$> matchPattern ctx p1 v1 <*> matchPattern ctx p2 v2
matchPattern _ _ _ = Nothing

bindPattern :: Bool -> Context -> [(Text, Value)] -> Pattern -> Value
bindPattern _ _ _ (PUnit) = VUnit
bindPattern _ _ _ (PBool b) = VBool b
bindPattern b ctx bs (PLeft p) = VLeft $ bindPattern b ctx bs p
bindPattern b ctx bs (PRight p) = VRight $ bindPattern b ctx bs p
bindPattern _ _ bs (PBind n) = case find ((==) n . fst) bs of
    Just (_, v) -> v -- We know that this is true
bindPattern b ctx bs (PProd p1 p2) = VProd (bindPattern b ctx bs p1) (bindPattern b ctx bs p2)
bindPattern inv ctx bs (PApp f p) = case lookupIso ctx f of
    Just i -> 
        if inv 
        then eval ctx (invert i) (bindPattern inv ctx bs p)
        else eval ctx i (bindPattern inv ctx bs p) 

invert :: Iso -> Iso
invert (n, t, ps) = (n, swap t, swap <$> ps)
    where
    swap :: (a,a) -> (a,a)
    swap (a,b) = (b,a)