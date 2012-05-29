#!/usr/bin/env runhaskell

{-
http://learnyouahaskell.com/recursion#quick-sort
-}

main :: IO ()
main = do print $ quicksort' [0, 23, 1, 34, 54, 1121, 0, 1121]

quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    (quicksort' lesseqs) ++ [x] ++ (quicksort' greaters)
    where
      lesseqs = filter (<=x) xs
      greaters = filter (>x) xs
