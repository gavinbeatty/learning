#!/usr/bin/env runhaskell
module Main where
main :: IO ()
main = do print $ take 100 $ myDropWhile (\x -> x `mod` 2 == 0) ([0,2..10] ++ [1,1..])

isEven :: Integral a => a -> Bool
isEven x = x `mod` 2 == 0

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile pred l@(x:xs)
    | pred x    = myDropWhile pred xs
    | otherwise = l

