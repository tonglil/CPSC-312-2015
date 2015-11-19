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
