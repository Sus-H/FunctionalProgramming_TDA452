module Expr where
import Prelude hiding (sin,cos)
import qualified Prelude as P
import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List


import Parsing
-- A
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
size X       = 0
size (UnOp _ b) = 1 + size b
size (BinOp _ b c) = 1 + size b + size c

-- B
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
    Num a -> map toLower (show trigOperator) ++ show a
    X     -> map toLower (show trigOperator) ++ " x"
    _  -> map toLower (show trigOperator) ++ "(" ++ showExpr expression ++ ")"

-- C
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

-- D
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

-- Parser for a double
number :: Parser Double
number = readsP 

-- E
-- Test that given an expression, which is put through our two functions
-- showExpr and readExpr will produce the original input
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr ex = case c of
    Just a  -> propHelp a == propHelp ex
    Nothing -> False
    where
        c = readExpr $ showExpr ex

propHelp :: Expr -> Expr
propHelp (Num a) = (Num a)
propHelp X       = x
propHelp (UnOp op a) = UnOp op (propHelp a)
propHelp (BinOp op a (BinOp op2 b c)) | op == op2 =
    propHelp (BinOp op (BinOp op (propHelp a) (propHelp b)) (propHelp c))
propHelp (BinOp op a b) = BinOp op (propHelp a) (propHelp b)

-- Generate an expression of limited size
arbExpr :: Int -> Gen Expr
arbExpr n | n == 0 = frequency [(1, Num <$> arbitrary), (1, return X)]
       | otherwise = frequency
        [(5, Num <$> arbitrary),
        (n, do
            a <- arbExpr (n `div` 2)
            b <- arbExpr (n `div` 2)
            op <- elements [Add, Mul]
            return $ BinOp op a b),
        (n `div` 2, do
            a <- arbExpr (n - 1)
            trigOp <- elements [Sin, Cos]
            return $ UnOp trigOp a),
        (5, do return X)]

instance Arbitrary Expr where 
  arbitrary = sized arbExpr

-- Examples
-- (BinOp Mul (UnOp Sin (Num 0)) (BinOp Add (Num 3) (Num 3)))
-- (BinOp Add (BinOp Mul (Num 1) (Num 0)) (x))

-- F
-- Simplifying expressions
simplify :: Expr -> Expr
simplify (Num a) = Num a
simplify X       = x

simplify (BinOp oper (Num a) (Num b)) = case oper of
    Add -> Num (a + b)
    Mul -> Num (a * b)
simplify (BinOp oper e (Num 0.0))     = case oper of 
    Add -> simplify e
    Mul -> Num 0
simplify (BinOp oper (Num 0.0) e)     = case oper of
    Add -> simplify e
    Mul -> Num 0

simplify (BinOp Mul e (Num 1.0)) = simplify e
simplify (BinOp Mul (Num 1.0) e) = simplify e

simplify (BinOp oper X X) = case oper of
    Add -> (BinOp Mul (Num 2) X)
    Mul -> (BinOp Mul X X)
simplify (BinOp oper (Num a) X) = case oper of
    Add -> (BinOp Add (Num a) X)
    Mul -> (BinOp Mul (Num a) X)
simplify (BinOp oper X (Num a)) = case oper of
    Add -> (BinOp Add X (Num a))
    Mul -> (BinOp Mul X (Num a))

simplify (BinOp Mul X e) = (BinOp Mul x (simplify e))
simplify (BinOp Mul e X) = (BinOp Mul (simplify e) x)

simplify (BinOp oper a b) = 
    let simplifiedA = simplify a
        simplifiedB = simplify b
    in case (oper, simplifiedA, simplifiedB) of
        (Add, Num c, Num d) -> Num (c+d)
        (Mul, Num c, Num d) -> Num (c*d)
        _                   -> BinOp oper simplifiedA simplifiedB

simplify (UnOp trig (Num a)) = case trig of
    Sin -> Num $ P.sin a
    Cos -> Num $ P.cos a
simplify (UnOp trig X) = (UnOp trig X)
simplify (UnOp t expr) = case simplify expr of
    Num a -> case t of
        Sin -> Num (P.sin a)
        Cos -> Num (P.cos a)
    simplifiedExpr -> UnOp t simplifiedExpr 

prop_simplifyEval :: Expr -> Double -> Bool
prop_simplifyEval a b = eval a b == eval (simplify a) b

prop_simplifyJunk :: Expr -> Bool
prop_simplifyJunk e = helperFunc simE && noJunk simE
    where
        simE = simplify e

helperFunc (Num a) = True
helperFunc X = True

helperFunc (BinOp _ (Num a) (Num b)) = False
helperFunc (BinOp op a b) = (helperFunc a) && (helperFunc b)

helperFunc (UnOp _ (Num a)) = False
helperFunc (UnOp _ X) = True
helperFunc (UnOp op a) = helperFunc a

noJunk :: Expr -> Bool
noJunk expr = not (all (isInfixOf (showExpr expr)) junks)
    where junks = ["+0.0", "0.0+", "*0.0", "0.0*", "1.0*", "*1.0"]

--Differentiate function and simplify expression
simplifyAndDifferentiate :: Expr -> Expr
simplifyAndDifferentiate expr = simplify $ differentiate expr

-- Differentiates an expression
differentiate :: Expr -> Expr
differentiate (Num _) = Num 0
differentiate X = Num 1

differentiate (BinOp Mul X X) = BinOp Mul (Num 2) X

differentiate (BinOp Add a b) =
    simplify $ BinOp Add (differentiate a) (differentiate b)

differentiate (BinOp Mul a b) =
    simplify $ BinOp Add (BinOp Mul (differentiate a) b)
        (BinOp Mul a (differentiate b))
differentiate (UnOp Sin expr) = 
    simplify $ BinOp Mul (differentiate expr) (UnOp Cos expr)

differentiate (UnOp Cos a) = 
    simplify $ BinOp Mul (Num (-1)) 
        (simplify $ BinOp Mul (differentiate a) (UnOp Sin a))
