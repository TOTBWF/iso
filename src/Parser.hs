{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Parser where

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
pattern = Pattern <$> uname <*> patternBody

patternBody :: TokenParser PatternBody
patternBody = foldr ($) Empty <$> P.sepBy p comma
    where
    p = const
        <|> P.try app
        <|> bind
        <|> nested
        <?> "Expected Pattern"
    const = Const <$> uname 
    bind = Bind <$> lname 
    app = App <$> lname <*> P.many1 lname 
    nested = parens $ Nested <$> pattern

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

runTokenParser :: FilePath -> TokenParser a -> [PositionedToken] -> Either P.ParseError a
runTokenParser filepath p = P.runParser p (ParseState 0) filepath 