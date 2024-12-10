import Prelude hiding (sin,cos)
import qualified Prelude as P

import Parsing

data Expr = Add Expr Expr 
          | Mul Expr Expr 
          | Sin Expr 
          | Cos Expr 
          | Num Double 
          | X 
    deriving (Eq, Show)

x :: Expr
x = X

num :: Double -> Expr
num a = Num a

add,mul :: Expr -> Expr -> Expr
add a b = Add a b
mul a b = Mul a b

sin,cos :: Expr -> Expr
sin a = Sin a
cos a = Cos a 

doubleParser = do
    -- natural <- sat elem ['1'..'9']
    -- nonNegative <- char '0' <|> natural
    nonNegative <- sat elem ['0'..'9']
    decimal <- char '.' 
    double <- digit <*> decimal <*> nonNegative
    return $ read double

doubleParser = do
    sign <- optional (char '-')
    integral <- some digitChar <|> string "0"
    fractional <- optional $ do
        decimal <- char '.'
        digits <- some digitChar
        return (decimal : digits)

-- Calculates the number of operations
size :: Expr -> Int
size (Num a) = 0
size (Sin a) = 1 + size a
size (Cos a) = 1 + size a
size (Add a b) = 1 + size a + size b
size (Mul a b) = 1 + size a + size b

-- Prints the expression
showExpr :: Expr -> String
showExpr (Num a)   = show a
showExpr X         = "x"
showExpr (Add a b) = showExpr a ++ " + " ++ showExpr b
showExpr (Mul a b) = showFactor a ++ " * " ++ showFactor b
   where showFactor (Add e e') = "(" ++ showExpr (Add e e')  ++ ")"
         showFactor e          = showExpr e
showExpr (Sin a) = "Sin(" ++ showExpr a ++ ")"
showExpr (Cos a) = "Cos(" ++ showExpr a ++ ")"

-- Evaluates an expression given the expression and the variable X
eval :: Expr -> Double -> Double
eval (Num a) _ = a
eval X b = b
eval (Add a b) c = eval a c + eval b c
eval (Mul a b) c = eval a c * eval b c
eval (Sin a) c = P.sin (eval a c)
eval (Cos a) c = P.cos (eval a c)

-- this does something I don't yet know what it is
readExpr :: String -> Maybe Expr
readExpr s = case parse expr s of
    Just (a,"") -> Just a
    _           -> Nothing
    where
        expr = foldl1 Add <$> chain term (char '+')
        term = foldl1 Mul <$> chain factor (char '*')
        factor = Num <$> number <|> char '(' *> expr <* char ')' 
             <|> siinn <|> cooss <|> xxx 
          where
            cooss = do
                w <- (trig "cos" factor)
                return $ Cos w
            siinn = do
                w <- trig "sin" factor
                return $ Sin w
            xxx = do
                char 'x'
                return x


trig :: String -> Parser Expr -> Parser Expr
trig s t = foldl1 (*>) [char c | c <- s] *> t

number :: Parser Double
number =  read <$> oneOrMore digit

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr = undefined

arbExpr :: Int -> Gen Expr
arbExpr = undefined