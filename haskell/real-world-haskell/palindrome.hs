#!/usr/bin/env runhaskell

isPal :: Eq a => [a] -> Bool
isPal [] = True
isPal [x] = True
isPal xs
    | half `mod` 2 == 0 = isPal' half xs
    | otherwise         = isPal' (half - 1) xs
        where half = (length xs) `quot` 2
              isPal' n xs = (take n xs) == (take n . reverse $ xs)

main = do print $ isPal "123"
          print $ isPal "12321"
          print $ isPal "123321"
