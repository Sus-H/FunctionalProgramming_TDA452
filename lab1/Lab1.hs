{- Lab 1
   Date: 
   Authors:
   Lab group:
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "Negative exponent not supported"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n 0         = 1
stepsPower n k | k > 0 = k + 1
               | otherwise = error "Negative exponent not supported"


-- B -------------------------
-- power1
power1 :: Integer -> Integer -> Integer
power1 n 0             = 1
power1 n k | k > 0     = product [ n | l <- [1..k] ]
           | otherwise = error "Negative exponent not supported"

-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n 0             = 1
power2 n k | even k    = (n * n) * power2 n (k `div` 2)
           | odd k     = n * power2 n (k-1)
           | otherwise = error "Negative exponent not supported"

-- D -------------------------
{- 

<Describe your test cases here>
   case 1: test with a negative number
   case 2: test with a zero
   case 3: test with a positive number greater than 0
 -}

-- 
prop_powers n k = power n k' == power1 n k'
   where k' = abs k

--
powerTest :: Bool
powerTest = undefined

--
prop_powers' = undefined