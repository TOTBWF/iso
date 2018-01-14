module Main where

import Control.Monad.State.Strict
import Control.Monad.Except

import qualified Data.Text as T
import Data.List (isPrefixOf, foldl')

import System.Exit
import System.Environment
import System.Console.Repline

import Syntax
import Lexer
import Parser

-- type Repl a = HaskelineT (StateT Context IO) a
type Repl a = HaskelineT IO a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

-- Commands

quit :: a -> Repl ()
quit _ = liftIO $ exitSuccess

help :: a -> Repl ()
help _ = liftIO $ do
    putStrLn "Top Level Commands:"
    putStrLn ":quit             Quits"
    putStrLn ":help             Prints this message"
    putStrLn ":debug            Prints debug information for a expression"

debug :: [String] -> Repl ()
debug args = do
    i <- hoistErr . runTokenParser "<stdin>" isomorphism . T.pack . unwords $ args
    liftIO . putStrLn $ "Input: " ++ show i

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    -- (":load"  , fileCompleter),
    -- (":dump"  , fileCompleter)
    ]

cmd :: [(String, [String] -> Repl ())]
cmd = 
    [ ("quit", quit)
    , ("help", help)
    , ("debug", debug)
    ]

comp :: (Monad m) => WordCompleter m
comp n = do
    let cmds = ((':':) . fst) <$> cmd
    return $ filter (isPrefixOf n) (cmds)
    
completer :: CompleterStyle IO
completer = Prefix (wordCompleter comp) defaultMatcher

main :: IO ()
main = evalRepl "Iso> " (\_ -> return ()) cmd completer (return ())