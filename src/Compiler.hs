module Compiler where

import System.Environment 
import Text.ParserCombinators.Parsec hiding (spaces) 
import Control.Monad
import Control.Monad.Error
import System.IO 

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
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of 
	    Bool False -> eval alt
	    otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecogonized special form" badForm

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg 
car badArgsList = throwError $ NumArgs 1 badArgsList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x 
cdr [badArg] = throwError $ TypeMismatch "pair" badArg 
cdr badArgsList = throwError $ NumArgs 1 badArgsList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]  
cons [x, List xs] = return $ List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgsList = throwError $ NumArgs 2 badArgsList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2 
eqv [(String arg1),(String arg2)] = return $ Bool $ arg1 == arg2 
eqv [(Atom arg1),(Atom arg2)] = return $ Bool $ arg1 == arg2 
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                              Left err -> False
			      Right (Bool val) -> val 
eqv [_, _] = return $ Bool False
eqv badArgsList = throwError $ NumArgs 2 badArgsList

--preparation for equal function that ignores types
data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unPackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unPackEquals arg1 arg2 (AnyUnpacker unpacker) =
               do
	         unpacked1 <- unpacker arg1
	         unpacked2 <- unpacker arg2
		 return $ unpacked1 == unpacked2
	       `catchError` (const $ return False)  	 

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
                     primitiveEquals <- liftM or $ mapM (unPackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unPackBool, AnyUnpacker unPackStr]    
		     eqvEquals <- eqv [arg1, arg2]
		     return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgsList = throwError $ NumArgs 2 badArgsList


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
               ("remainder", numericBinop rem),
               ("=", numBoolBinop (==)),
               ("<", numBoolBinop (<)),
               (">", numBoolBinop (>)),
               ("/=", numBoolBinop (/=)),
               (">=", numBoolBinop (>=)),
               ("<=", numBoolBinop (<=)),
               ("&&", boolBoolBinop (&&)),
               ("||", boolBoolBinop (||)),
               ("string=?", strBoolBinop (==)),
               ("string<?", strBoolBinop (<)),
               ("string>?", strBoolBinop (>)),
               ("string<=?", strBoolBinop (<=)),
               ("string>=?", strBoolBinop (>=)),
	       ("car", car),
	       ("cdr", cdr),
	       ("cons", cons),
	       ("eq?", eqv),
	       ("eqv?", eqv),
	       ("equal?", equal)]
               
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] ->ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 [] 
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal 
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op 

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args  
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1  
				     return $ Bool $ left `op` right 

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unPackBool
strBoolBinop = boolBinop unPackStr

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                         if null parsed
			    then throwError $ TypeMismatch "number" $ String n 
			    else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unPackBool :: LispVal -> ThrowsError Bool
unPackBool (Bool b) = return b
unPackBool notBool = throwError $ TypeMismatch "boolean" notBool

unPackStr :: LispVal -> ThrowsError String
unPackStr (String s) = return s
unPackStr (Number s) = return $ show s
unPackStr (Bool s) = return $ show s
unPackStr notString = throwError $ TypeMismatch "string" notString


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

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval) 

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

--first monadic function
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
                              result <- prompt
			      if pred result
			      then return ()
			      else action result >> until_ pred prompt action

--awesome composition
runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

main :: IO ()
main = do
         args <- getArgs
	 case length args of
	   0 -> runRepl
	   1 -> evalAndPrint $ args !! 0
	   otherwise -> putStrLn "Program takes only 0 or 1 arguments."

{-
main :: IO()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled 
-}

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
