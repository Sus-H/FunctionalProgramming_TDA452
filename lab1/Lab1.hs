{- Lab 1
   Date: 6/11 -2024
   Authors: Susanne On Huang, Paulina Palmberg
   Lab group: 7
 -}
--------------------------------------------
import Test.QuickCheck

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
power2 n k | even k    = power2 (n * n) (k `div` 2)
           | odd k     = n * power2 n (k-1)
           | otherwise = error "Negative exponent not supported"

-- D -------------------------
{- 

<Describe your test cases here>
   Well behaved test cases:
   case 1: test with a zero
   case 2: test with an even positive k greater than 0
   case 2: test with an odd positive k greater than 0
   These are cases that will cover the different cases 
   in the function definitions.
 -}

-- 
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k' == power1 n k' 
              && power1 n k' == power2 n k' 
              && power2 n k' == power n k'
   where k' = abs k

--
powerTest :: Bool
powerTest = and [prop_powers n k | (n,k) <- [(1,0), (2,2), (2,3)]]

--
prop_powers' :: Integer -> Integer -> Bool
prop_powers' = prop_powers