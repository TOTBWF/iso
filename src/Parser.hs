{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Parser where

import Prelude hiding (lex)
import Control.Applicative ((<|>))
import Text.Parsec ((<?>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as PX
import Text.Parsec.Expr (buildExpressionParser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (foldl')
import Data.Functor.Identity

import Syntax
import Lexer

type Op a = PX.Operator [PositionedToken] ParseState Identity a
type Operators a = PX.OperatorTable [PositionedToken] ParseState Identity a

infixOp :: Text -> (a -> a -> a) -> PX.Assoc -> Op a
infixOp x f = PX.Infix (reserved x >> return f)

atyp :: TokenParser Type
atyp = P.choice
    [ P.try $ lcurly *> rcurly *> pure TVoid
    , P.try $ lparen *> rparen *> pure TUnit
    , P.try $ reserved "Bool" *> pure TBool
    , P.try $ TName <$> uname
    ]

typTable :: Operators Type
typTable = 
    [
        [ infixOp "*" (\t1 t2 -> TProd t1 t2) PX.AssocLeft ],
        [ infixOp "+" (\t1 t2 -> TSum t1 t2) PX.AssocLeft ]
    ]

typ :: TokenParser Type
typ = PX.buildExpressionParser typTable atyp

pattern :: TokenParser Pattern
pattern = pat >>= \p ->
    (P.sepBy1 pat comma >>= \ps -> return $ foldl' PProd p ps)
    <|> return p
    where
    -- | Parses a single pattern instance
    pat :: TokenParser Pattern
    pat = P.choice
        [ number' 1 *> pure (PBool True)
        , number' 0 *> pure (PBool False)
        , P.try $ lparen *> rparen *> pure PUnit
        , P.try $ PLeft <$> (reserved "Left" *> pattern)
        , P.try $ PRight <$> (reserved "Right" *> pattern)
        , PBind <$> identifier
        , parens pattern
        ]

isomorphism :: TokenParser Iso
isomorphism = do
    reserved "iso"
    i <- identifier
    doublecolon
    t1 <- typ
    doublearrow
    t2 <- typ
    ps <- P.many1 (pipe >> (,) <$> pattern <* doublearrow <*> pattern) 
    return $ Iso i (t1, t2) ps

typedef :: TokenParser TypeDef
typedef = do
    reserved "type"
    n <- uname
    equals
    t <- typ
    return (n, t)

runTokenParser :: FilePath -> TokenParser a -> Text -> Either P.ParseError a
runTokenParser filepath p t = (P.runParser p (ParseState 0) filepath) =<< lex filepath t
