import Prelude hiding (sin,cos)
import qualified Prelude as P

import Parsing

data Expr = Add Expr Expr 
          | Mul Expr Expr 
          | Sin Expr 
          | Cos Expr 
          | Num Double 
          | X 

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
readExpr = expr
    where
        expr = foldl1 Add <$> chain term (char '+')
        term = foldl1 Mul <$> chain factor (char '*')
        factor = Num <$> number <|> char '(' *> expr <* char ')' -- <|> Sin och cos och siffror

number :: Parser Integer
number = read <$> oneOrMore digit