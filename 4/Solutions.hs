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

-- Question 3

-- Remove duplicated elements from a list to only keep the last instance.
myremoveduplicates :: Eq a => [a] -> [a]
myremoveduplicates list
    -- The base case
    | null list                     = list
    -- The recursive step: the first element exists in the rest of the list, so the answer is the result of recursing with the rest of the list
    | elem (head list) (tail list)  = myremoveduplicates (tail list)
    -- The recursive step: the first element does not exist in the rest of the list, so the answer is a list constructed with this element and the result of recursing with the tail of the list
    | otherwise                     = (head list) : myremoveduplicates (tail list)

-- Remove duplicated elements from a list to only keep the last instance using pattern matching.
myremoveduplicates_pm :: Eq a => [a] -> [a]
myremoveduplicates_pm [] = []                                           -- The base case
myremoveduplicates_pm (x : xs) | elem x xs = myremoveduplicates_pm xs   -- 
myremoveduplicates_pm (x : xs) = x : myremoveduplicates_pm xs           -- The recursive step: 

mynthtail :: Int -> [a] -> [a]
mynthtail n list
    | n == 0 = list
    | otherwise = mynthtail (n - 1) (tail list)

mynthtail_pm :: Int -> [a] -> [a]
mynthtail_pm 0 list = list
mynthtail_pm x (_ : ys) = mynthtail_pm (x - 1) (ys)

myordered :: Ord a => [a] -> Bool
myordered list
    | null list = True
    | null (tail list) = True
    | head list <= head (tail list) = myordered (tail list)
    | otherwise = False

myordered_pm :: Ord a => [a] -> Bool
myordered_pm [] = True
myordered_pm [_] = True
myordered_pm (x : y : xs) | x <= y = myordered_pm(y : xs)
myordered_pm (_ : _) = False

myreplaceall :: Eq a => a -> a -> [a] -> [a]
myreplaceall x y list
    | null list = []
    | y == head list = x : (myreplaceall x y (tail list))
    | otherwise = head list : (myreplaceall x y (tail list))

myreplaceall_pm :: Eq a => a -> a -> [a] -> [a]
myreplaceall_pm _ _ [] = []
myreplaceall_pm x y (z : xs) | y == z = x : myreplaceall_pm x y xs
myreplaceall_pm x y (z : xs) = z : myreplaceall_pm x y xs

-- The set of all available coins to use.
coins :: [Int]
coins = [100,50,20,10,5,2,1]

-- Compute the smallest set of coins to make up an amount.
change :: Int -> [Int]
-- The base case
change 0 = []
-- The recursive step using list comprehension
change total =
    [
        x |
        let cs = [y | y <- coins, y <= total],      -- A list of coins that are each smaller than the current total
        let biggest = head (cs),                    -- The current coin to add to the set of coins
        x <- (biggest : change (total - biggest))   -- Generate a list of coins with the current coin and list of coins found by recursing with the remaining amount
    ]

