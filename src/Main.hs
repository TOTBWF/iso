module Main where

import Control.Monad.State.Strict
import Control.Monad.Except

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (isPrefixOf, foldl')

import System.Exit
import System.Environment
import System.Console.Repline

import Syntax
import Lexer
import Parser
import TypeCheck
import Context
import Pretty
import Eval

-- type Repl a = HaskelineT (StateT Context IO) a
type Repl a = HaskelineT (StateT Context IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

define :: Text -> Repl ()
define line = do
    ctx <- get
    decl <- hoistErr $ parseDecl "<stdin>" line
    case decl of
        (TypeDecl t) -> do
            put $ extendType ctx t
        (IsoDecl i) -> do
            hoistErr $ runInfer ctx $ check i
            put $ extendIso ctx i

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
    liftIO . putStrLn $ "AST: " ++ show i

typeof :: [String] -> Repl ()
typeof args = do
    ctx <- get
    case lookupIso ctx (T.pack $ unwords args) of
        Just (_, (t1, t2), _) -> liftIO . putStrLn $ (unwords args) ++ " :: " ++ ppType emptyCtx t1 ++ " <-> " ++ ppType emptyCtx t2
        Nothing -> liftIO . putStrLn $ "Error: " ++ (unwords args) ++ " is not defined"

context :: a -> Repl ()
context _ = do
    ctx <- get
    liftIO . putStrLn $ (show ctx)

eval :: Bool -> [String] -> Repl ()
eval inv args = do
    ctx <- get
    p <- hoistErr . parsePattern . T.pack . unwords $ args
    liftIO $ putStrLn $ ppValue ctx $ bindPattern inv ctx [] p

load :: [String] -> Repl ()
load args = do
    contents <- liftIO $ T.readFile (unwords args)
    prog <- hoistErr $ parseFile (unwords args) contents
    mapM_ (extendDecl) prog
    where
    extendDecl :: Decl -> Repl ()
    extendDecl (TypeDecl t) = do
        ctx <- get
        put $ extendType ctx t
        return ()
    extendDecl (IsoDecl i) = do
        ctx <- get
        hoistErr $ runInfer ctx $ check i 
        put $ extendIso ctx i
        return ()

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = 
    [ (":load"  , fileCompleter) ]

cmd :: [(String, [String] -> Repl ())]
cmd = 
    [ ("quit", quit)
    , ("help", help)
    , ("debug", debug)
    , ("typeof", typeof)
    , ("context", context)
    , ("evall", eval False)
    , ("evalr", eval True)
    , ("load", load)
    ]

comp :: (Monad m) => WordCompleter m
comp n = do
    let cmds = ((':':) . fst) <$> cmd
    return $ filter (isPrefixOf n) (cmds)
    
completer :: CompleterStyle (StateT Context IO)
completer = Prefix (wordCompleter comp) defaultMatcher

main :: IO ()
main = flip evalStateT emptyCtx
    $ evalRepl "Iso> " (define . T.pack) cmd completer (return ())