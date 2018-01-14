{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-matches -fno-warn-orphans #-}
module Lexer where

import Prelude hiding (lex)

import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT
import Data.Functor.Identity

-- We want to use the Parsec lexeme support, but have control over the tokenization stage
data Token
    = LParen
    | RParen
    | LCurly
    | RCurly
    | Pipe
    | Equals
    | Comma
    | Dollar
    | DoubleArrow
    | DoubleColon
    | Indent Int
    | LName Text
    | UName Text
    | Symbol Text
    | Number Integer
    deriving (Show, Eq, Ord)

type Comment = Text
data PositionedToken = PositionedToken
  {
    psourcePos :: P.SourcePos
  , pendPos :: P.SourcePos
  , ptoken :: Token
  , pcomments :: [Comment]
  }
  deriving (Show)

ppToken :: Token -> Text
ppToken (LParen)      = "("
ppToken (RParen)      = ")"
ppToken (LCurly)      = "{"
ppToken (RCurly)      = "}"
ppToken (Equals)      = "="
ppToken (Pipe)        = "|"
ppToken (Comma)       = ","
ppToken (Dollar)      = "$"
ppToken (DoubleArrow) = "<->"
ppToken (DoubleColon) = "::"
ppToken (Indent n)    = "Indentation at level " <> (T.pack $ show n)
ppToken (LName n)     = n 
ppToken (UName n)     = n
ppToken (Symbol s)    = s
ppToken (Number n)    = T.pack (show n)

-- | Used to lex our input into a token stream
type Lexer u a = P.Parsec Text u a

lex :: FilePath -> Text -> Either P.ParseError [PositionedToken]
lex f s = P.parse parseTokens f s

whitespace :: Lexer u ()
whitespace = P.skipMany (P.satisfy isSpace)

parseComment :: Lexer u Comment
parseComment = P.try $ P.string "--" *> (T.pack <$> P.manyTill P.anyChar (P.try (void $ P.char '\n') <|> P.eof))

parseTokens :: Lexer u [PositionedToken]
parseTokens = whitespace *> P.many parsePositionedToken <* P.skipMany parseComment <* P.eof

parsePositionedToken :: Lexer u PositionedToken
parsePositionedToken = P.try $ do
    comments <- P.many parseComment
    pos <- P.getPosition
    tok <- parseToken
    pos' <- P.getPosition
    whitespace
    return $ PositionedToken pos pos' tok comments

-- | Parses a single token
parseToken :: Lexer u Token
parseToken = P.choice
    [ P.try $ P.string "<->" *> P.notFollowedBy symbolChar *> pure DoubleArrow
    , P.try $ P.string "::" *> P.notFollowedBy symbolChar *> pure DoubleColon
    , P.try $ P.char '(' *> pure LParen
    , P.try $ P.char ')' *> pure RParen
    , P.try $ P.char '{' *> pure LCurly
    , P.try $ P.char '}' *> pure RCurly
    , P.try $ P.char '=' *> pure Equals
    , P.try $ P.char '|' *> pure Pipe
    , P.try $ P.char ',' *> pure Comma
    , P.try $ P.char '$' *> pure Dollar
    , LName <$> parseLName
    , UName <$> parseUName
    , Symbol <$> parseSymbol
    , Number <$> parseNumber
    ]
    where

    parseLName :: Lexer u Text
    parseLName = T.cons <$> identStart <*> (T.pack <$> P.many identLetter)

    parseUName :: Lexer u Text
    parseUName = T.cons <$> P.upper <*> (T.pack <$> P.many identLetter)

    parseSymbol :: Lexer u Text
    parseSymbol = T.pack <$> P.many1 symbolChar

    parseNumber :: Lexer u Integer
    parseNumber = PT.natural tokenizer

    identStart :: Lexer u Char
    identStart = P.lower P.<|> P.oneOf "_"
    
    identLetter :: Lexer u Char
    identLetter = P.alphaNum P.<|> P.oneOf "_'" 

    symbolChar :: Lexer u Char
    symbolChar = P.satisfy isSymbolChar
   
isSymbolChar :: Char -> Bool
isSymbolChar c = c `elem` ("!@#$%^&+*-=\\|:<>/" :: [Char])

-- | We use Parsec lexeme support, but none of the other features
lang :: PT.GenLanguageDef Text u Identity
lang = PT.LanguageDef
    { PT.reservedNames   = []
    , PT.reservedOpNames = []
    , PT.commentStart    = ""
    , PT.commentEnd      = ""
    , PT.commentLine     = ""
    , PT.nestedComments  = True
    , PT.identStart      = P.parserFail "Identifiers not supported"
    , PT.identLetter     = P.parserFail "Identifiers not supported"
    , PT.opStart         = P.parserFail "Identifiers not supported"
    , PT.opLetter        = P.parserFail "Identifiers not supported"
    , PT.caseSensitive   = True
    }

tokenizer :: PT.GenTokenParser Text u Identity
tokenizer = PT.makeTokenParser lang

data ParseState = ParseState {
        indentation :: P.Column
    }

-- | Parses a token stream produced by the Lexer
type TokenParser = P.ParsecT [PositionedToken] ParseState Identity

-- | Used to convert our custom tokens into a Parsec Parser
token :: (Token -> Maybe a) -> TokenParser a
token f = P.tokenPrim showTok nextPos testTok
    where
    showTok = T.unpack . ppToken . ptoken
    nextPos pos _ (tok:_) = psourcePos tok
    nextPos pos _ [] = pos
    testTok = f . ptoken

-- | Tries to parse a matching token from the token stream
match :: Token -> TokenParser ()
match tok = token (\tok' -> if tok' == tok then Just () else Nothing) P.<?> T.unpack (ppToken tok)

lparen :: TokenParser ()
lparen = match LParen

rparen :: TokenParser ()
rparen = match RParen

lcurly :: TokenParser ()
lcurly = match LCurly

rcurly :: TokenParser ()
rcurly = match RCurly

parens :: TokenParser a -> TokenParser a
parens = P.between lparen rparen

curlies :: TokenParser a -> TokenParser a
curlies = P.between lcurly rcurly

doublecolon :: TokenParser ()
doublecolon = match DoubleColon

equals :: TokenParser ()
equals = match Equals

pipe :: TokenParser ()
pipe = match Pipe

comma :: TokenParser ()
comma = match Comma

dollar :: TokenParser ()
dollar = match Dollar

indent :: TokenParser Int
indent = token go P.<?> "indentation"
    where
    go (Indent n) = Just n
    go _ = Nothing

doublearrow :: TokenParser ()
doublearrow = match DoubleArrow

lname :: TokenParser Text
lname = token go P.<?> "identifier"
    where
    go (LName n) = Just n
    go _ = Nothing

uname :: TokenParser Text
uname = token go P.<?> "proper name"
    where
    go (UName n) = Just n
    go _ = Nothing

symbol :: TokenParser Text
symbol = token go P.<?> "symbol"
    where
    go (Symbol s) = Just s
    go _ = Nothing

number :: TokenParser Integer
number = token go P.<?> "number"
    where
    go (Number n) = Just n
    go _ = Nothing

identifier :: TokenParser Text
identifier = token go P.<?> "identifier"
    where
    go (LName n) | n `notElem` reservedNames = Just n
    go _ = Nothing

reserved :: Text -> TokenParser ()
reserved s = token go P.<?> show s
    where
    go (LName s') | s == s' = Just ()
    go (UName s') | s == s' = Just ()
    go (Symbol s') | s == s' = Just ()
    go _ = Nothing

reservedNames :: [Text]
reservedNames = [ "type" 
                , "iso"
                , "Bool"
                ]