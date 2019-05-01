
module Parser where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.IORef


type Env = IORef [(String, IORef Val)]

type Throw = Either Error

data Error =
    NumArg Integer [Val]
    |TypeMismatch String Val
    |Parser ParseError
    |BadSpecialForm String Val
    |NotFunction String String
    |UnboundVar String String
    |Default String

data Val = Atom String
    | List [Val]
    | DottedList [Val] Val
    | Number Integer
    | String String
    | Bool Bool
    | PrimitiveFun ([Val] -> Throw Val)
    | Fun { params :: [String], vararg :: (Maybe String), 
            body :: [Val], closure :: Env}


--want to ignore white space
spaces :: Parser ()
spaces = skipMany1 space
--
--parser will tokenize those as 'symbol'
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--we can look for each datatype
--strings dont support escaped " or stuff like \n yet
parseString :: Parser Val
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
--return lets us return it as a Parser type
    return (String x)


--we find atoms here using choice operator <|>
parseAtom :: Parser Val
parseAtom = do
    x <- letter <|> symbol
    xs <- many (letter <|> digit <|> symbol)
    let atom = x:xs
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom 


--parse numbers useing parsec and it takes digits,
--then converts string to numbers then returns it
parseNumber :: Parser Val
parseNumber = 
    (many1 digit) >>= \x -> return (Number (read x))

--we parse expressions here
parseExpr :: Parser Val
parseExpr    = parseAtom 
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> do 
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseList :: Parser Val
--sepby is separated by, its like *
parseList = (sepBy parseExpr spaces) >>= \x -> return (List x)


parseDottedList :: Parser Val
parseDottedList = do
    --think of it like * in regexp 
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return (DottedList head tail)
--lisp has opposite kind of list? it does list body and tail

parseQuoted :: Parser Val
parseQuoted = do
    char '\''
    x <- parseExpr
    return (List [Atom "quote", x])

--to show our values
showVal :: Val -> String
showVal (String xs) = "\"" ++ xs ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ (unwords.map showVal) xs ++ ")"
showVal (DottedList xs x) = "(" ++ (unwords.map showVal) xs ++ " . " ++ showVal x ++ ")"
showVal (PrimitiveFun _) = "<primitive>"
showVal (Fun {params = args, vararg = varargs, body = body, closure = env}) =
    "(\\ (" ++ unwords (map show args) ++ (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)" 

--we make an instance of show so that we can display stuff
instance Show Val where show = showVal

