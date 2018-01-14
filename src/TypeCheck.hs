module TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad (mapM_)
import Data.Text (Text)
import Data.List (union)

import Syntax

data TypeError
    = Debug String
    | UndefinedTypeError Text
    | IsomorphicTypeError Type Type
    | MismatchError Type Type
    | PatternMatchError Pattern Type
    | BindingError 
    | VoidError Pattern
    deriving (Show)

type Infer = ExceptT TypeError (Reader Context)

runInfer :: Context -> Infer a -> Either TypeError a
runInfer ctx i = runReader (runExceptT i) ctx

-- infer :: Iso -> Infer (Type, Type)
-- infer (Sym i) = do
--     (t1, t2) <- infer i
--     return (t2, t1)
-- infer (Compose i1 i2) = do
--     (t1, t2) <- infer i1
--     (t3, t4) <- infer i2
--     if t2 == t3 then
--         return (t1, t4)
--     else
--         throwError $ MismatchError t2 t3
-- infer _ = throwError $ Debug "TODO: User Type Inference"

-- | Attempts to bind each free variable in a pattern to a type
bindVars :: Pattern -> Type -> Infer [(Text, Type)]
bindVars p t = case (p,t) of
    (_, TVoid) -> throwError $ VoidError p
    (PBool _, TBool) -> return []
    (PUnit , TUnit) -> return []
    (PLeft p', TSum t1 _) -> bindVars p' t1
    (PRight p', TSum _ t2) -> bindVars p' t2
    (PBind n, _) -> return [(n, t)]
    (PProd p1 p2, TProd t1 t2) -> union <$> bindVars p1 t1 <*> bindVars p2 t2
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
check (Iso _ t ps) = mapM_ (checkCase t) ps

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
isomorphismCheck :: Type -> Type -> Infer ()
isomorphismCheck t1 t2 = do
    n1 <- size t1
    n2 <- size t2
    if (n1 /= n2) then throwError $ IsomorphicTypeError t1 t2
    else return ()
