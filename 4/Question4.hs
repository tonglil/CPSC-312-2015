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
