myremoveduplicates :: Eq a => [a] -> [a]
myremoveduplicates list
    | null list                            = list
    | elem (head list) (tail list) == True = myremoveduplicates (tail list) 
    | otherwise                            = (head list) : myremoveduplicates (tail list) 

myremoveduplicates_pm :: Eq a => [a] -> [a]
myremoveduplicates_pm [] = []
myremoveduplicates_pm (x : xs) | elem x xs = myremoveduplicates_pm xs
myremoveduplicates_pm (x : xs) = x : myremoveduplicates_pm xs

mynthtail :: Int -> [a] -> [a]
mynthtail n list
    | n == 0 = list
    | otherwise = mynthtail (n - 1) (tail list)

mynthtail_pm :: Int -> [a] -> [a]
mynthtail_pm 0 list = list
mynthtail_pm x (y : ys) = mynthtail_pm (x - 1) (ys)

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