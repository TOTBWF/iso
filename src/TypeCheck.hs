module TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad (mapM_)
import Data.Text (Text)
import Data.List (union)

import Syntax
import Context

data TypeError
    = Debug String
    | UndefinedTypeError Text
    | UndefinedIsoError Text
    | IsomorphicTypeError Type Type
    | MismatchError Type Type
    | PatternMatchError Pattern Type
    | CoverageError [Pattern]
    | MultiMatchError Pattern [Pattern]
    | BindingError 
    | VoidError Pattern
    deriving (Show)

type Infer = ExceptT TypeError (Reader Context)

runInfer :: Context -> Infer a -> Either TypeError a
runInfer ctx i = runReader (runExceptT i) ctx

-- | Attempts to bind each free variable in a pattern to a type
bindVars :: Pattern -> Type -> Infer [(Text, Type)]
bindVars p t = case (p,t) of
    (_, TVoid) -> throwError $ VoidError p
    (PBool _, TBool) -> return []
    (PUnit , TUnit) -> return []
    (PLeft p', TSum t1 _) -> bindVars p' t1
    (PRight p', TSum _ t2) -> bindVars p' t2
    (PApp n p', _) -> do
        ctx <- ask
        case lookupIso ctx n  of
            Just (_, (t1, t2), _) -> 
                if t2 /= t then throwError $ MismatchError t t2
                else bindVars p' t1
            Nothing -> throwError $ UndefinedIsoError n

    (PBind n, _) -> return [(n, t)]
    (PProd p1 p2, TProd t1 t2) -> union <$> bindVars p1 t1 <*> bindVars p2 t2
    (p, TName n) -> do
        ctx <- ask
        case lookupType ctx n of
            Just t -> bindVars p t
            Nothing -> throwError $ UndefinedTypeError n
    (_, _) -> throwError $ PatternMatchError p t

-- | Checks to see that a case is valid
checkCase :: (Type, Type) -> (Pattern, Pattern) -> Infer ()
checkCase (t1, t2) (p1, p2) = do
    b1 <- bindVars p1 t1
    b2 <- bindVars p2 t2
    if b1 == b2 then return ()
    else throwError $ BindingError

-- | Checks to see if an isomorphism declaration is valid
check :: Iso -> Infer ()
check (_, (t1, t2), ps) = do
    checkIsomorphism t1 t2
    checkCoverage (fmap fst ps) t1
    checkCoverage (fmap snd ps) t2
    mapM_ (checkCase (t1, t2)) ps 

-- | Computes the free variable set of a pattern
freeVars :: Pattern -> Infer [Text]
freeVars (PLeft p) = freeVars p
freeVars (PRight p) = freeVars p
freeVars (PProd p1 p2) = (union) <$> freeVars p1 <*> freeVars p2
freeVars (PBind n) = return [n]
freeVars _ = return []

-- | Computes the number of inhabitants of the type
size :: Type -> Infer Int
size (TVoid) = return 0
size (TUnit) = return 1
size (TBool) = return 2
size (TSum t1 t2) = do
    n1 <- size t1 
    n2 <- size t2
    return (n1 + n2)
size (TProd t1 t2) = do
    n1 <- size t1 
    n2 <- size t2
    return (n1 * n2)
size (TName n) = do
    ctx <- ask
    case lookupType ctx n of
        Just t -> size t
        Nothing -> throwError $ UndefinedTypeError n

-- | Checks that 2 types can be isomorphic
checkIsomorphism :: Type -> Type -> Infer ()
checkIsomorphism t1 t2 = do
    n1 <- size t1
    n2 <- size t2
    if (n1 /= n2) then throwError $ IsomorphicTypeError t1 t2
    else return ()

-- | Generates the set of all possible patterns
generatePatterns :: Type -> Infer [Pattern]
generatePatterns TVoid = return []
generatePatterns TUnit = return [PUnit]
generatePatterns TBool = return [PBool True, PBool False]
generatePatterns (TSum t1 t2) = do
    p1 <- generatePatterns t1
    p2 <- generatePatterns t2
    return (fmap PLeft p1 ++ fmap PRight p2)
generatePatterns (TProd t1 t2) = do
    p1 <- generatePatterns t1
    p2 <- generatePatterns t2
    return [ PProd v1 v2 | v1 <- p1, v2 <- p2 ]
generatePatterns (TName n) = do
    ctx <- ask
    case lookupType ctx n of
        Just t -> generatePatterns t
        Nothing -> throwError $ UndefinedTypeError n

-- | Checks to see if 2 patterns will reconcile
-- | Assumes that the 2nd pattern consists of no free variables
reconcile :: Pattern -> Pattern -> Bool
reconcile (PUnit) (PUnit) = True
reconcile (PBool b1) (PBool b2) = b1 == b2
reconcile (PLeft p1) (PLeft p2) = reconcile p1 p2
reconcile (PRight p1) (PRight p2) = reconcile p1 p2
reconcile (PProd p1 p2) (PProd p3 p4) = reconcile p1 p3 && reconcile p2 p4
reconcile (PBind _) _ = True
reconcile _ (PBind _) = True
reconcile (PApp _ _) _ = True -- Assumes that functions always work, not true
reconcile _ (PApp _ _) = True -- Assumes that functions always work, not true
reconcile _ _ = False


checkCoverage :: [Pattern] -> Type -> Infer ()
checkCoverage ps t = do
    ps' <- generatePatterns t
    res <- foldM (constrict) ps' (reverse ps)
    case res of
        [] -> return ()
        xs -> throwError $ CoverageError xs
    where
    
    -- | Constricts down to coverage set
    constrict :: [Pattern] -> Pattern -> Infer [Pattern]
    constrict ps p = 
        let ps' = filter (not . reconcile p) ps
        in if ps == ps' then throwError $ MultiMatchError p ps else return ps'


