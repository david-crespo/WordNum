import Data.List (sort, nub, delete, group)
import qualified Data.Map as Map

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

-- Element counts (e.g., [2,2,1]) to the number of unique
-- permutations. The number of possibilities if there
-- were no duplicates is (2 + 2 + 1) factorial. Then we
-- divide by 2!, 2! and 1! to account for the duplicates.
perms' :: [Integer] -> Integer
perms' [] = 1
perms' l = fact total `div` duplication where
    total = sum l
    duplication = product $ map fact l
    fact n = product [1..n]

-- given a list of elements, create a list of element counts
-- e.g., "ababc" produces the list [2,2,1] because there are
-- 2 As, 2 Bs, and 1 C
eltCounts :: Ord a => [a] -> [Integer]
eltCounts = (map (toInteger . length)) . group . sort

-- remove old from the list and put new in. we don't
-- bother to put new at the same index because we don't
-- care about the order here
replace :: Eq a => a -> a -> [a] -> [a]
replace new old list = new:(delete old list)


-----------------------------------
-- Testing
-----------------------------------

answers = Map.fromList [("abab", 2),
                        ("aaab", 1),
                        ("baaa", 4),
                        ("question", 24572),
                        ("bookkeeper", 10743)]

test = if allWordsCorrect then putStrLn "pass" else putStrLn "fail" where
    allWordsCorrect = all testWord (Map.keys answers)

testWord :: String -> Bool
testWord word = (myAnswer == rightAnswer) where
    rightAnswer = answers Map.! word
    myAnswer = wordNum word

verboseTest = map verboseTestWord (Map.keys answers)

verboseTestWord :: String -> (String, Integer, Integer)
verboseTestWord word = (word, myAnswer, rightAnswer) where
    rightAnswer = answers Map.! word
    myAnswer = wordNum word

