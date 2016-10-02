module Main where
import Control.Monad
import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMisMatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal

parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many digit 
parseNumber = do numberStr <- many1 digit
                 let num = read numberStr
                 return $ Number num

parseList :: Parser LispVal
parseList = do list <- sepBy parseExpr spaces
               return $ List list

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]




parseExpr :: Parser LispVal
parseExpr =      parseAtom
             <|> parseString
             <|> parseNumber
             <|> parseQuoted
             <|> do char '('
                    x <- (try parseList) <|> parseDottedList
                    char ')'
                    return x

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"String: " ++ contents ++ "\""
showVal (Atom name) = "Atom: " ++ name
showVal (Number contents) = "Number: " ++ show contents
showVal (Bool True) = "Bool: #t"
showVal (Bool False) = "Bool: #f"
showVal (List contents) = "List: (" ++ unwordList contents ++ ")"
showVal (DottedList head tail) = "DottedList: (" ++ unwordList head ++ showVal tail ++ ")"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (NumArgs expected found) = "Excepted " ++ show expected ++ " args: found values " ++ unwordList found
showError (TypeMisMatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError


unwordList :: [LispVal] -> String
-- unwordList contents = unwords $ map showVal contents 
unwordList = unwords . map showVal

spaces :: Parser ()
spaces = skipMany1 space


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

eval :: LispVal -> ThrowsError LispVal
eval (Atom val) = return $ Atom val
eval (String val) = return $ String val
eval (Number val) = return $ Number val
eval (Bool val) = return $ Bool val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do result <- eval pred
                                                case result of
                                                  Bool False -> eval alt
                                                  otherwise -> eval conseq
                                                  
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($ args) $ lookup func primitives

primitives :: [(String, ([LispVal] -> ThrowsError LispVal))]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
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
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("symbol?", isSymbol),
              ("number?", isNumber),
              ("car", car)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right
                                        
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool



numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
-- numericBinop op params = return . Number $ foldl1 op $ map unpackNum params
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

--isTrue :: LispVal -> LispVal

car ::[LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMisMatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [(Atom _)] = return $ Bool True
isSymbol _ = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [(String _)] = return $ Bool True
isString _ =return $ Bool False

isNumber :: [LispVal] ->ThrowsError LispVal
isNumber [(Number _)] = return $ Bool True
isNumber _ = return $ Bool False

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                         if null parsed
                            then throwError $ TypeMisMatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMisMatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMisMatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMisMatch "boolean" notBool

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

main :: IO ()
main = do args <- getArgs
          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
          putStrLn $ extractValue $ trapError evaled
