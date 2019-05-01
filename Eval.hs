module Eval where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except
import Data.IORef
import Parser

--we declare an Env type that hold environments for us

blankEnv :: IO Env
blankEnv = newIORef []

type IOThrow = ExceptT Error IO

--lifting stuff from eval into env + err monad
liftThrows :: Throw a -> IOThrow a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

--get the error out of an IOThrow and turn it into a IO string
runIOThrow :: IOThrow String -> IO String
runIOThrow action = runExceptT (trapError action) >>= return . extractVal

--define show for errors 
showError :: Error -> String
showError (NumArg exp fnd) = "Expected " ++ show exp ++ " arguments, but found " ++ show (length fnd)
showError (TypeMismatch exp fnd) = "Invalid type, expected " ++ exp ++ " , but found " ++ show fnd
showError (Parser perr) = "Parse error at " ++ show perr
showError (BadSpecialForm msg form) = msg ++ ":" ++ show form
showError (NotFunction msg fun) = msg ++ ":" ++ show fun
showError (UnboundVar msg var) = msg ++ ":" ++ var


--define the instance so that show is implemented
instance Show Error where show = showError


--catches error 
trapError action = catchError action (return . show)
--extracts the right data only intend to use it after caught error
--no left handling
extractVal :: Throw a -> a
extractVal (Right v) = v

--returns a monadic either where right is good val, left is error
readExpr :: String -> Throw Val
readExpr input = case parse (parseExpr) "lisp" input of
    Left err -> throwError (Parser err)
    Right x -> return x

--some funky stuff, line 2 takes head and reads it, passes it to eval
--then shows it and returns it as monad, and 3rd line prints it
{-
main :: IO ()
main = do
    args <- getArgs 
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractVal $ trapError evaled
-}

eval :: Env -> Val -> IOThrow Val
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = get env id
--quote is 2 item list
eval env (List [Atom "quote", val]) = return val
--if statements | evaluate the predicate and bind it to result,
--then execute depending on result
eval env (List [Atom "if", pred, res, alt]) = 
    eval env pred >>= (\result -> case result of
            Bool False -> eval env alt
            Bool True -> eval env res
            otherwise -> throwError (TypeMismatch "boolean" result))
--predefinied things so setting vars and defining functions
eval env (List [Atom "set!", Atom var, form]) = 
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = 
    eval env form >>= defVar env var
eval env (List (Atom "define" : List (Atom var : params) :body)) = 
    makeNormalFun env params body >>= defVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs :body)) = 
    makeVarArgs varargs env params body >>= defVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFun env params body
eval env (List (Atom "lambda" : DottedList params varargs : body )) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
--list of expressions after function - function application
eval env (List (fun : exps)) = do
    func <- eval env fun
    argval <- mapM (eval env) exps
    apply func argval
--throw error
eval env badForm = throwError (BadSpecialForm "unrecognized form" badForm )




--apply looks for stuff in primitives that corresponds to String
--and that gets evaled
apply :: Val -> [Val] -> IOThrow Val
apply (PrimitiveFun fun) exps = liftThrows $ fun exps
apply (Fun params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
        then throwError (NumArg (num params) args)
        else (liftIO $ bindVar closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger.length
          evalBody env = liftM last (mapM (eval env) body)
          bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVar env [(argName, List $ remainingArgs)]
                Nothing -> return env


--a list of strings that map functions that take lists of ins
primitives ::[(String, [Val] -> Throw Val)]
primitives = [("+", binaryOp (+)),
              ("-", binaryOp (-)),
              ("*", binaryOp (*)),
              ("/", binaryOp div),
              ("symbol?", symbolQuestion),
              ("%", binaryOp mod),
              ("=", numBinOp (==)),
              (">", numBinOp (>)),
              ("<", numBinOp (<)),
              ("/=", numBinOp (/=)),
              (">=", numBinOp (>=)),
              ("<=", numBinOp (<=)),
              (">=", numBinOp (>=)),
              ("&&", boolBinOp (&&)),
              ("||", boolBinOp (||)),
              ("string=?", strBinOp (==)),
              ("string<?", strBinOp (<)),
              ("string>?", strBinOp (>)),
              ("string<=?", strBinOp (<=)),
              ("string>=?", strBinOp (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", eqv)
              ]
--can add quotient and remainder

--takes the operation and folds overeach one
binaryOp :: (Integer -> Integer -> Integer) -> [Val] -> Throw Val
binaryOp oper []    = throwError (NumArg 2 [])
binaryOp oper singleVal@[_]   = throwError (NumArg 2 singleVal)
binaryOp oper params = mapM unpackNum params >>= return . Number . foldl1 oper

--match numbers to themselves and everything else to 0

--implements symbol? which tells you if its a symbol
symbolQuestion :: [Val] -> Throw Val
symbolQuestion [] = throwError (NumArg 1 [])
-- = Bool (foldl (&&) True (map symbolQuestion' xs)) 
symbolQuestion xs = mapM symbolQuestion' xs >>= return . Bool . foldl (&&) True

symbolQuestion' :: Val -> Throw Bool
symbolQuestion' (Atom n) = return True
symbolQuestion' _ = return False

-- implementing binary operations
binOp :: (Val -> Throw a) -> (a -> a -> Bool) -> [Val] -> Throw Val
binOp unpack oper exps = if length exps/= 2 then throwError (NumArg 2 (exps)) 
    else do 
        left <- unpack (exps !! 0)
        right <- unpack (exps !! 1)
        return (Bool (left `oper` right))

boolBinOp = binOp unpackBool
strBinOp = binOp unpackStr
numBinOp = binOp unpackNum

--just a few funcs that return types 
unpackBool :: Val -> Throw Bool
unpackBool (Bool a) = return a
unpackBool bad = throwError (TypeMismatch "boolean" bad)

unpackStr :: Val -> Throw String 
unpackStr (String a) = return a
unpackStr bad = throwError (TypeMismatch "string" bad)

unpackNum :: Val -> Throw Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError (TypeMismatch "number" notNum)


--list primitives
car:: [Val] -> Throw Val
car [List (x:xs)]       = return x
car [DottedList (x:xs) _] = return x
car [doesntmatch]       = throwError (TypeMismatch "pair" doesntmatch)
car badArgument         = throwError (NumArg 1 badArgument)

cdr :: [Val] -> Throw Val
cdr [List (x:xs)]       = return (List xs)
cdr [DottedList [_] x]  = return x
cdr [DottedList (_:xs) x]  = return (DottedList xs x)
cdr [doesntmatch]       = throwError (TypeMismatch "pair" doesntmatch)
cdr badArgument         = throwError (NumArg 1 badArgument)

cons :: [Val] -> Throw Val
cons [x, List[]]        = return (List [x])
cons [x, List[xs]]      = return (List (x:[xs]))
cons [x, DottedList xs xlast]   = return (DottedList (x:xs) xlast)
cons [x1, x2]           = return (DottedList [x1] x2)

eqv :: [Val] -> Throw Val
--first 4 are just copied over
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)]= eqv [(List (xs ++ [x])), (List (ys ++ [y]))]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair (zip arg1 arg2))
    where eqvPair (x,y) = case eqv [x, y] of
             Left err -> False
             Right (Bool val) -> val
eqv [_, _]                                  =return (Bool False)
eqv bad                                     = throwError (NumArg 2 bad)



--looks up a string in the enviroment using readIORef and lookup
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const  True) . lookup var

--gets a value from the env ref
get :: Env -> String -> IOThrow Val
get envRef var = do
    env <- liftIO (readIORef envRef)
    maybe (throwError (UnboundVar "unbound variable" var)) 
        (liftIO . readIORef) 
        (lookup var env)


--set variables
setVar :: Env -> String -> Val -> IOThrow Val
setVar envRef var val = do
    env <- liftIO (readIORef envRef)
    maybe (throwError (UnboundVar "setting unbound var" var)) 
        (liftIO. (flip writeIORef val)) 
        (lookup var env) 
    return val


--define variables in env
--if value exists then set it, 
--else we create new entry
defVar :: Env -> String -> Val -> IOThrow Val
defVar envRef var val = do
    defined <- liftIO (isBound envRef var)
    if defined then setVar envRef var val >> return val
    else liftIO $ do
        valueRef <- newIORef val
        env <- readIORef envRef
        writeIORef envRef ((var, valueRef) :env)
        return val

--bind a bunch of vars at once
bindVar :: Env -> [(String, Val)] -> IO Env
bindVar  envRef bindings = readIORef envRef >>= extendsEnv bindings >>= newIORef
    where extendsEnv bindings env = liftM (++ env) (mapM addBindings bindings)
          addBindings (var, val) = newIORef val >>= (\ref -> return (var, ref))

-- just a way to make vars and such into function types
makeFun varargs env params body = return $ Fun (map showVal params) varargs body env
makeNormalFun = makeFun Nothing
makeVarArgs = makeFun . Just . showVal


