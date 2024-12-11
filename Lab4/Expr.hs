import Prelude hiding (sin,cos)
import qualified Prelude as P
import Test.QuickCheck
import Data.Maybe
import Data.Char

import Parsing

data Expr = BinOp Op Expr Expr 
          | UnOp TrigOp Expr
          | Num Double 
          | X 
    deriving (Eq, Show)

data Op = Add | Mul
    deriving (Eq, Show)
data TrigOp = Sin | Cos 
    deriving (Eq, Show)

x :: Expr
x = X

num :: Double -> Expr
num a = Num a

add,mul :: Expr -> Expr -> Expr
add a b = BinOp Add a b
mul a b = BinOp Mul a b

sin,cos :: Expr -> Expr
sin a = UnOp Sin a
cos a = UnOp Cos a 

-- Calculates the number of operations
size :: Expr -> Int
size (Num a) = 0
size (UnOp _ b) = 1 + size b
size (BinOp _ b c) = 1 + size b + size c

-- Prints the expression (turns an expression to a String)
showExpr :: Expr -> String
showExpr (Num a)   = show a
showExpr X         = "x"
-- showExpr (Add a b) = showExpr a ++ " + " ++ showExpr b
-- showExpr (Mul a b) = showFactor a ++ " * " ++ showFactor b
showExpr (BinOp c a b) = case c of 
    Mul -> showFactor a ++ " * " ++ showFactor b
    _   -> showExpr a ++ " + " ++ showExpr b
   where
        showFactor (BinOp Add e e') = "(" ++ showExpr (BinOp Add e e')  ++ ")"
        showFactor e          = showExpr e
showExpr (UnOp trigOperator expression) = case expression of
    Num a -> show trigOperator ++ show a
    X     -> show trigOperator ++ "x"
    _     -> show trigOperator ++ "(" ++ showExpr expression ++ ")"

-- Evaluates an expression given the expression and the variable X
eval :: Expr -> Double -> Double
eval (Num a) _ = a
eval X b = b
eval (BinOp a b c) d = case a of
    Add -> eval b d + eval c d
    Mul -> eval b d * eval c d
eval (UnOp a b) c = case a of
    Sin -> P.sin (eval b c)
    Cos -> P.cos (eval b c)

-- -- Turns a String into an expression
readExpr :: String -> Maybe Expr
readExpr s = case parse expr $ filter (/=' ') $ toLower <$> s of
    Just (a,"") -> Just a
    _           -> Nothing
    where
        expr = foldl1 (BinOp Add) <$> chain term (char '+') 
        term = foldl1 (BinOp Mul) <$> chain factor (char '*')
        factor = Num <$> number <|> char '(' *> expr <* char ')' 
             <|> siinn <|> cooss <|> xxx 
          where
            cooss = do
                w <- (trig "cos" factor)
                return $ UnOp Cos w
            siinn = do
                w <- trig "sin" factor
                return $ UnOp Sin w
            xxx = do
                char 'x'
                return x


trig :: String -> Parser Expr -> Parser Expr
trig s t = foldl1 (*>) [char c | c <- s] *> t

-- charCaseInsensitive :: Char -> Parser Char
-- charCaseInsensitive c = satisfy (\x -> toLower x == toLower c)

-- Parser for a double
number :: Parser Double
number = read <$> do
    ds   <- oneOrMore digit
    rest <- decimals <|> return ""
    return (ds ++ rest)
    where
        decimals = do 
            c      <- char '.'
            smalls <- oneOrMore digit
            return (c:smalls)

-- Test that given an expression, which is put through our two functions
-- showExpr and readExpr will produce the original input
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr ex = case c of 
    Just _  -> True
    Nothing -> False
    where
        c = readExpr $ showExpr ex

-- Generate an expression of limited size
arbExpr :: Int -> Gen Expr
arbExpr n = frequency
    [(20, Num <$> elements[0..10]), 
     (15, do
        a <- arbExpr (n `div` 2)
        b <- arbExpr (n `div` 2)
        op <- elements [Add, Mul]
        return $ BinOp op a b),
     (1, do
        a <- arbExpr (n - 1)
        trigOp <- elements [Sin, Cos]
        return $ UnOp trigOp a),
     (10, do return X)]

instance Arbitrary Expr where 
  arbitrary = sized arbExpr

simplify :: Expr -> Expr
simplify e = undefined

