module Question2
    where

harmonicNatural :: Int -> Float
harmonicNatural n
    | n == 1    = harm
    | otherwise = harm + harmonicNatural (n - 1)
        where
        harm = 1 / fromIntegral n

harmonicTail :: Int -> Float
harmonicTail n
