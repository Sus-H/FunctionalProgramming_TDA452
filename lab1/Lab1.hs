{- Lab 1
   Date: 
   Authors:
   Lab group:
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = undefined 


-- B -------------------------
-- power1

power1 = undefined

-- C -------------------------
-- power2

power2 = undefined

-- D -------------------------
{- 

<Describe your test cases here>

 -}

-- 
prop_powers = undefined

--
powerTest :: Bool
powerTest = undefined

--
prop_powers' = undefined