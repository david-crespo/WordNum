import Data.List (sort, nub, delete, group)
import qualified Data.Map as Map

-- see readme for a top-level explanation
wordNum :: Ord a => [a] -> Integer
wordNum [] = 1
wordNum (x:xs) = totalLowerPerms + (wordNum xs) where
    uniqLowers = nub $ filter ((>) x) xs
    totalLowerPerms = sum [permsStartingWith letter (x:xs) | letter <- uniqLowers]

-- the number of unique permutations of a word starting with a
-- particular letter in that word is simply the total number
-- of permutations of the rest of the letters in that word
permsStartingWith :: Ord a => a -> [a] -> Integer
permsStartingWith letter word = perms wordMinusLetter where
    wordMinusLetter = delete letter word

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

