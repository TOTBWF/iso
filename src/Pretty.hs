{-# LANGUAGE FlexibleInstances #-}
module Pretty where

import Text.PrettyPrint

import Syntax
import Context

typedef :: Doc 
typedef = colon <> colon

class Pretty p where
    ppr :: Context -> p -> Doc

instance Pretty Type where
    ppr _ TVoid = text "{}"
    ppr _ TUnit = text "()"
    ppr _ TBool = text "Bool"
    ppr ctx (TSum t1 t2) = ppr ctx t1 <+> text "+" <+> ppr ctx t2
    ppr ctx (TProd t1 t2) = ppr ctx t1 <+> text "*" <+> ppr ctx t2
    ppr ctx (TName n) = case lookupType ctx n of
        Just t -> ppr ctx t
        Nothing -> text "_"

-- instance Pretty (Type, Type) where
--     ppr ctx (t1, t2) = ppr ctx t1 <+> text "<->" <+> ppr ctx t2
ppType :: Context -> Type -> String
ppType ctx = render . ppr ctx