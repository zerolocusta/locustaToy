defn max [a b]
	(println a b)
	(if (> a b) a b))module Main where
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

data LispError = NumAgs Integer [LispVal]
               | TypeMisMatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String
               | Default String

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
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordList head ++ showVal tail ++ ")"

unwordList :: [LispVal] -> String
-- unwordList contents = unwords $ map showVal contents 
unwordList = unwords . map showVal

spaces :: Parser ()
spaces = skipMany1 space


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

eval :: LispVal -> LispVal
eval (Atom val) = Atom val
eval (String val) = String val
eval (Number val) = Number val
eval (Bool val) = Bool val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: (String -> [LispVal] -> LispVal)
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, ([LispVal] -> LispVal))]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isSymbol),
              ("number?", isNumber),
              ("string?", isString)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

--isTrue :: LispVal -> LispVal

isSymbol :: [LispVal] -> LispVal
isSymbol [(Atom _)] = Bool True
isSymbol _ = Bool False

isString :: [LispVal] -> LispVal
isString [(String _)] = Bool True
isString _ = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [(Number _)] = Bool True
isNumber _ = Bool False

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
                         if null parsed
                            then 0
                            else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No Match: " ++ show err
                   Right val -> val

main :: IO ()
main = do args <- getArgs
          putStrLn (show $ eval $ readExpr (args !! 0))