{-# LANGUAGE FlexibleInstances #-}
module Pretty where

import Text.PrettyPrint
import Data.Text as T

import Syntax
import Context
import TypeCheck

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

instance Pretty Pattern where
    ppr _ PUnit = text "()" 
    ppr _ (PBool True) = text "1"
    ppr _ (PBool False) = text "0"
    ppr ctx (PLeft p) = text "Left" <> parens (ppr ctx p)
    ppr ctx (PRight p) = text "Right" <> parens (ppr ctx p)
    ppr _ (PBind t) = text $ T.unpack t
    ppr ctx (PApp t p) = text (T.unpack t) <> parens(ppr ctx p)
    ppr ctx (PProd p1 p2) = ppr ctx p1 <> comma <+> ppr ctx p2

instance Pretty TypeError where
    ppr _ (Debug s) = text s
    ppr _ (UndefinedTypeError t) = text "Undefinded Type Error:" <+> text (T.unpack t)
    ppr _ (UndefinedIsoError  t) = text "Undefinded Isomorphism Error:" <+> text (T.unpack t)
    ppr ctx (IsomorphicTypeError t1 t2) = text "Type Isomorphism Error:" <+> ppr ctx t1 <+> text "and" <+> ppr ctx t2 <+> text "are not isomorphic!"
    ppr ctx (MismatchError t1 t2)= text "Type Mismatch Error:" <+> ppr ctx t1 <+> text "and" <+> ppr ctx t2
    ppr ctx (PatternMatchError p t) = text "Pattern Match Error:" <+> ppr ctx p <+> text "and" <+> ppr ctx t
    ppr ctx (CoverageError ps) = text "Pattern Converage Error:" $$ vcat (ppr ctx <$> ps)
    ppr ctx (MultiMatchError p ps) = text "Multiple Match Error:" <+> ppr ctx p $$ vcat (ppr ctx <$> ps)
    ppr _ (BindingError) = text "Binding Error"
    ppr ctx (VoidError p) = text "Void Error:" <+> ppr ctx p

pp :: Pretty p => Context -> p -> String
pp ctx = render . ppr ctx
