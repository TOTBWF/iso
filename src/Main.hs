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

prettyError :: Pretty e => Context -> Either e a -> Repl a
prettyError _ (Right val) = return val
prettyError ctx (Left err) = do
    liftIO $ putStrLn $ pp ctx err
    abort

dumpError :: Show e => Either e a -> Repl a
dumpError (Right val) = return val
dumpError (Left err) = do
    liftIO $ print err
    abort

define :: Text -> Repl ()
define line = do
    ctx <- get
    decl <- dumpError $ parseDecl "<stdin>" line
    case decl of
        (TypeDecl t) -> do
            put $ extendType ctx t
        (IsoDecl i) -> do
            prettyError ctx $ runInfer ctx $ check i
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
    p <- dumpError . parsePattern . T.pack . unwords $ args
    -- i <- hoistErr . runTokenParser "<stdin>" isomorphism . T.pack . unwords $ args
    liftIO . putStrLn $ "AST: " ++ show p

typeof :: [String] -> Repl ()
typeof args = do
    ctx <- get
    case lookupIso ctx (T.pack $ unwords args) of
        Just (_, (t1, t2), _) -> liftIO . putStrLn $ (unwords args) ++ " :: " ++ pp emptyCtx t1 ++ " <-> " ++ pp emptyCtx t2
        Nothing -> liftIO . putStrLn $ "Error: " ++ (unwords args) ++ " is not defined"

context :: a -> Repl ()
context _ = do
    ctx <- get
    liftIO . putStrLn $ (show ctx)

eval :: Bool -> [String] -> Repl ()
eval inv args = do
    ctx <- get
    p <- dumpError . parsePattern . T.pack . unwords $ args
    liftIO $ putStrLn $ pp ctx $ bindPattern inv ctx [] p

load :: [String] -> Repl ()
load args = do
    contents <- liftIO $ T.readFile (unwords args)
    prog <- dumpError $ parseFile (unwords args) contents
    mapM_ (extendDecl) prog
    where
    extendDecl :: Decl -> Repl ()
    extendDecl (TypeDecl t) = do
        ctx <- get
        put $ extendType ctx t
        return ()
    extendDecl (IsoDecl i) = do
        ctx <- get
        prettyError ctx $ runInfer ctx $ check i 
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