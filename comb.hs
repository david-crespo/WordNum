import Data.List (sort, nub, delete, group)

{-|
Consider the word { x_1, x_2, ... , x_n }. This solution is based
on the the idea that the word number will be the sum of two
quantities: (1) the number of combinations starting with letters
lower in the alphabet than x_0, and (2) how far we are into the
words starting with x_0. The second quantity happens to be the
word number of the word { x_2, ... , x_n }. This points to a
recursive implementation.
-}
wordNum :: Ord a => [a] -> Integer
wordNum [] = 1
wordNum (x:xs) = totalLowerPerms + (wordNum xs) where
    uniqLowers = nub $ filter ((>) x) xs
    totalLowerPerms = sum permsForEachLower
    permsForEachLower = map perms [replace x lowLet xs | lowLet <- uniqLowers]

-- unique perms of letters in a word
perms :: Ord a => [a] -> Integer
perms = perms' . eltCounts

-- Element counts (e.g., [2,2,1]) to uniq perms count
perms' :: [Integer] -> Integer
perms' [] = 1
perms' (a:as) = firstLetterPerms * perms' as where
    firstLetterPerms = slots `choose` a where
        slots = sum (a:as)

-- How many unique ways there are to choose k things
-- from a set of n where the order is irrelevant
choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 

-- given a list of elements, create a list of element counts
-- e.g., "ababc" produces the list [2,2,1] because there are
-- 2 As, 2 Bs, and 1 C
eltCounts :: Ord a => [a] -> [Integer]
eltCounts = (map (toInteger . length)) . group . sort

-- Remove old from list, put new in. Order is irrelevant.
replace :: Eq a => a -> a -> [a] -> [a]
replace new old list = new:(delete old list)
