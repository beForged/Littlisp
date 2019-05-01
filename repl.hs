module Main where
import Eval
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except
import Data.IORef
import System.IO
import Parser

flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

nullEnv ::IO Env
nullEnv = newIORef []


primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVar $ map makePrimFun primitives)
    where makePrimFun (var, fun) = (var, PrimitiveFun fun)

evalString :: Env -> String -> IO String
evalString env expr = runIOThrow $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalandprint :: Env -> String -> IO()
evalandprint env expr = evalString env expr >>= putStrLn

blank :: String -> IO ()
blank str = primitiveBindings >>= flip evalandprint str


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result 
        then return () 
        else action result >> until_ pred prompt action

repl :: IO ()
repl = primitiveBindings >>= until_ (=="quit") (readPrompt "Littlisp>") . evalandprint

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> repl
        1 -> blank (args !! 0)
        otherwise -> putStrLn "0 or 1 arguments"

