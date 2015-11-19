{-
   Byron Duenas
   34095117
   v5e8

   Tongli Li
   15688112
   w6d8
-}

-- Notice: all functions must have explicit type declaration.
-- Notice: all functions must have comments.

module Solutions
    where

-- Question 1

kungPaoFactor
    :: Int
    -> Float
    -> Float
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Float
-- Calculate the Kung Pao Factor to determine whether or not to order Chinese take out.
-- This formula is the sum of two terms.
kungPaoFactor r dm ds n c ft ff s =
    term1 + term2
    where
        -- Use fromIntegral to convert an Int to a Float for calculation
        term1 = fromIntegral n / 30 - ds / dm
        term2 = 10 * (fromIntegral s ** 2) * sqrt (fromIntegral r) / fromIntegral (c * (ft - ff + 1))

-- Question 2

-- The natural recursion method calls the same function and builds on the call stack until the recursion terminates.
harmonicNatural :: Int -> Float
harmonicNatural n
    | n == 1    = harm                              -- The base case
    | otherwise = harm + harmonicNatural (n - 1)    -- The recursive step
        where
        harm = 1 / fromIntegral n

-- The tail recursion method calls a helper function with an accumulator.
harmonicTail :: Int -> Float
harmonicTail n = harmonicHelper n 1 -- The accumulator has the result of the first harmonic sum

-- Add the result of the previous sumations to the current n-th harmonic sequence to get the current sum.
-- Then call recursively with the next decreasing integer in the sequence and the current sum (as the accumulator) until the recursion terminates.
harmonicHelper :: Int -> Float -> Float
harmonicHelper n total
    | n == 1    = total                                                 -- The base case
    | otherwise = harmonicHelper (n - 1) (total + 1 / fromIntegral n)   -- The recursive step
