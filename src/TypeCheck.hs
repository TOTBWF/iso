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
check (_, (t1, t2), ps) = mapM_ (checkCase (t1, t2)) ps <* checkIsomorphism t1 t2

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
