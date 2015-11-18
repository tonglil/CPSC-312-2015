module Question1
    where

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
-- Option 1
-- kungPaoFactor r dm ds n c ft ff s =
--     term1 + term2
--     where
--         term1 = fromIntegral n / 30 - ds / dm
--         term2 = 10 * (fromIntegral s ** 2) * sqrt (fromIntegral r) / fromIntegral (c * (ft - ff + 1))

-- Option 2
kungPaoFactor r dm ds n c ft ff s =
    term1 + term2
        where
        term1 = fromIntegral n / 30 - ds / dm
        term2 = 10 * stress / fromIntegral (c * food)
            where
            stress = (fromIntegral s ** 2) * sqrt (fromIntegral r)
            food = ft - ff + 1
