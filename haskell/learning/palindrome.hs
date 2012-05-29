-- #!/usr/bin/env runhaskell

import Data.List

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

makePalindrome :: [a] -> [a]
makePalindrome [] = []
makePalindrome list = list ++ (myReverse list)

listEq :: Eq a => [a] -> [a] -> Bool
listEq (a:as) (b:bs)
    | a == b    = listEq as bs
    | otherwise = False
listEq _ _ = True

-- instance List Eq where
--     (==) = listEq
--     (/=) = (not . listEq)

testPalindrome :: Eq a => [a] -> Bool
testPalindrome xs = (revFirstHalf `listEq` secondHalf) || ((drop 1 revFirstHalf) `listEq` secondHalf)
    where secondHalf = drop half xs
          revFirstHalf = reverse (take half xs)
          half = (length xs) `div` 2

main = print (map testPalindrome ["dad", "daad", "pooop", "foo"])
