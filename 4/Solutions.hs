module Assign4 where 

--------- Question 1
kungPaoFactor
  :: Float
     -> Float
     -> Float
     -> Float
     -> Float
     -> Float
     -> Float
     -> Float
     -> Float

kungPaoFactor r dm ds n c ft ff s =
  (term1 dm ds n) + (term2 r c ft ff s)

term1 dm ds n = (n / 30) - (ds / dm)

term2 r c ft ff s = (term2top s r) / (term2bottom c ft ff)

term2top s r = 10 * (s ** 2) * (sqrt r)

term2bottom c ft ff = c * (ft - ff + 1)

--------- Question 2

-- a.
harmonic :: Int -> Float
harmonic n
 | n == 1    = 1.0
 | otherwise = 1.0/(fromIntegral n) + harmonic (n - 1)


-- b.
harmonic_tr :: Int -> Float
harmonic_tr n = harmonic_tr' n 1.0

harmonic_tr' :: Int -> Float -> Float
harmonic_tr' n result
 | n == 1    = result
 | otherwise = 
     harmonic_tr' (n - 1) (1.0/(fromIntegral n) + result) 


--------- Question 3
myremoveduplicates_pm :: (Eq a) => [a] -> [a]
myremoveduplicates_pm [] = []
myremoveduplicates_pm (x:xs) 
 | x `elem` xs = myremoveduplicates_pm xs
 | otherwise = x:(myremoveduplicates_pm xs)


myremoveduplicates :: (Eq a) => [a] -> [a]
myremoveduplicates inlist 
 | null inlist  = []
 | otherwise = if (head inlist) `elem` (tail inlist) then myremoveduplicates (tail inlist)
else (head inlist):(myremoveduplicates (tail inlist))

mynthtail_pm :: Int -> [a] -> [a]
mynthtail_pm 0 inlist = inlist
mynthtail_pm n [] = []
mynthtail_pm n (_:xs) = mynthtail_pm (n-1) xs

mynthtail :: (Eq a) => Int -> [a] -> [a] 
mynthtail n inlist 
 | n == 0 || inlist == [] = inlist
 | otherwise = mynthtail (n-1) (tail inlist)


myordered_pm :: (Ord a) => [a] -> Bool
myordered_pm (x:y:xs) = if x <= y then myordered (y:xs) else False 
-- or optionally, can write: = x < y && myoreder (y:xs)
myordered_pm inlist = True

myordered :: (Ord a) => [a] -> Bool
myordered inlist 
 | null inlist || null (tail inlist) = True
 | otherwise = h1 <= h2 && myordered t
 where h1 = head inlist; t = tail inlist; h2 = head t

myreplaceall_pm :: (Eq a) => a -> a -> [a] -> [a]
myreplaceall_pm x y [] = []
myreplaceall_pm x y (z:xs) = (if y==z then x else z):(myreplaceall_pm x y xs)

myreplaceall :: (Eq a) => a -> a -> [a] -> [a] 
myreplaceall x y inlist
 | inlist==[] = []
 | z==y = x:rest
 | otherwise = z:rest 
  where z = head inlist; rest = myreplaceall x y (tail inlist)

------------ Question 4
-- the recursive implementation
change n coins
  | coins==[] && n==0 = []
  | n >= h = h:(change (n-h) coins)
  | otherwise = change n t
  where h=head coins; t=tail coins

-- list comprehension
optimal_change n = head $ all_change n

all_change 0 = [[]]
all_change n = [c:cs | c<- coins, n>=c, cs<- all_change (n-c)]

coins = [100,50,20,10,5,2,1] 
-- the coins list have to be sorted from highest to lowest to ensure 
-- the most optimal answer is at the head
-- if the order is reversed it will be at the last position in all_change

