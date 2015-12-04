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

-- Get the last n elements of the list
mynthtail :: Int -> [a] -> [a]
mynthtail n list
    -- The base case
    | n == 0 = list
    -- The recursive step: call mynthtail with the tail of the list and decrement n
    | otherwise = mynthtail (n - 1) (tail list)

-- Get the last n elements of the list through pattern matching
mynthtail_pm :: Int -> [a] -> [a]
-- The base case: if the first parameter is 0 return the list
mynthtail_pm 0 list = list
-- The recursive step: call mynthtail_pm with x - 1 and the tail of the list 
mynthtail_pm x (_ : ys) = mynthtail_pm (x - 1) (ys)

-- Check if the list is ordered
myordered :: Ord a => [a] -> Bool
myordered list
    -- Base case: if the list is empty return true
    | null list = True
    -- Base case: If the list only has one element return true
    | null (tail list) = True
    -- The recursive step: If the head of the list is less than the greater than head of the tail call myordered with the tail
    | head list <= head (tail list) = myordered (tail list)
    -- Return False otherwise 
    | otherwise = False

-- Check if the list is ordered using pattern matching
myordered_pm :: Ord a => [a] -> Bool
-- Base case: Matches empty list pattern, return True
myordered_pm [] = True
-- Base case: Matches single element list pattern, return True
myordered_pm [_] = True
-- The recursive step: Matches pattern where first element is less than or equal to second element then calls my ordered with tail of list
myordered_pm (x : y : xs) | x <= y = myordered_pm(y : xs)
-- Return False otherwise
myordered_pm (_ : _) = False

-- Replace every list element of the second parameter with the first parameter
myreplaceall :: Eq a => a -> a -> [a] -> [a]
myreplaceall x y list
    -- The base case: if the list is empty return an empty list
    | null list = []
    -- Recursive step: if the head of the list equals the second parameter, call myreplaceall with the tail, prepended with the first parameter
    | y == head list = x : (myreplaceall x y (tail list))
    -- Recursive step: if the head of the list doesn't equals the second parameter, call myreplaceall with the tail, prepended with original head of the list
    | otherwise = head list : (myreplaceall x y (tail list))

-- Replace every list element of the second parameter with the first parameter using pattern matching
myreplaceall_pm :: Eq a => a -> a -> [a] -> [a]
-- The base case: Matches empty list pattern, return an empty list
myreplaceall_pm _ _ [] = []
-- Recursive step: Matches pattern that second parameter equals head of third parameter, call myreplaceall with the tail, prepended with the first parameter
myreplaceall_pm x y (z : xs) | y == z = x : myreplaceall_pm x y xs
-- Recursive step: Matches pattern that second parameter doesn't equal head of third parameter, call myreplaceall with the tail, prepended with original head of the list
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

