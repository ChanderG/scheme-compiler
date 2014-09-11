module Compiler where

import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces) 
import Control.Monad
import Control.Monad.Error

--Data types and tokens handling
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ case atom of 
                          "#t" -> Bool True                        
                          "#f" -> Bool False                
                          otherwise -> Atom atom

spaces :: Parser ()
spaces = skipMany1 space

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [ Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom 
            <|> parseString 
            <|> parseNumber 
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
		   char ')'
		   return x

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val 
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecogonized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecogonized primitive function args" func) 
                        ($ args) 
			(lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives  = [("+", numericBinop(+)),
               ("-", numericBinop(-)),
               ("*", numericBinop(*)),
               ("/", numericBinop div),
               ("mod", numericBinop mod),
               ("quotient", numericBinop quot),
               ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] ->ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 [] 
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal 
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op 

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                         if null parsed
			    then throwError $ TypeMismatch "number" $ String n 
			    else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

--TODO: make the testExpr a function of this readExpr - if the next line is needed
--if you update this do update testExpr function also
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr  "lisp" input of
    Left err -> throwError $ Parser err 
    Right val -> return val

--special function to make it easy to unit test
--just ensure that is is updated
testExpr :: String -> String
testExpr input = case parse parseExpr  "lisp" input of
    Left err -> "No match"
    Right val -> "Found " ++ show val

--Error handling
data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser ParseError
              | BadSpecialForm String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func  --is this right?
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args;found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ " but found, " ++ show found 
showError (Parser parseErr) = "Parse error at: " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occured"
  strMsg = Default

--awesome use of partial application
type ThrowsError = Either LispError

trapError action = catchError action (return . show)

--designed to work only with correct values
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

--Main
--main :: IO()
--main = getArgs >>= print . eval . readExpr . head 

main :: IO()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled 

--statements of do need to be aligned
--main = do args <- getArgs
         -- putStrLn(readExpr $ args !! 0)
          {-
          args <- getArgs
          argst <- getArgs  --any number of getArgs gets the same args passed in 
          putStrLn("Hello " ++ args !! 0)
          putStrLn("Hello " ++ argst !! 1)
          -}
          
          --empty lines in between work 
          {-
          putStrLn "Enter your name: "
          name <- getLine
          putStrLn("Hello, " ++ name) 
          -} 

          {-
          args <- getArgs
          putStrLn("Bye " ++  show(read(args !! 0) + 10))
          -} 
