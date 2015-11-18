module Question2
    where

harmonicNatural :: Int -> Float
harmonicNatural n
    | n == 1    = harm
    | otherwise = harm + harmonicNatural (n - 1)
        where
        harm = 1 / fromIntegral n

harmonicTail :: Int -> Float
harmonicTail n = harmonicHelper n 1

harmonicHelper :: Int -> Float -> Float
harmonicHelper n total
    | n == 1    = total
    | otherwise = harmonicHelper (n - 1) (total + 1 / fromIntegral n)
