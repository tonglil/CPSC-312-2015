change :: Int -> [Int]
change 0 = []
change total =[x |let coins = [y | y <- [100,50,20,10,5,2,1], y <= total], let biggest = head (coins), x <- (biggest:change(total - biggest))]