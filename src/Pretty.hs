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
    
instance Pretty Value where
    ppr _ VUnit = text "()"
    ppr _ (VBool True) = text "1"
    ppr _ (VBool False) = text "0"
    ppr ctx (VLeft v) = text "Left" <> parens (ppr ctx v)
    ppr ctx (VRight v) = text "Right" <> parens (ppr ctx v)
    ppr ctx (VProd v1 v2) = ppr ctx v1 <> comma <+> ppr ctx v2

-- instance Pretty (Type, Type) where
--     ppr ctx (t1, t2) = ppr ctx t1 <+> text "<->" <+> ppr ctx t2
ppType :: Context -> Type -> String
ppType ctx = render . ppr ctx

ppValue :: Context -> Value -> String
ppValue ctx = render . ppr ctx