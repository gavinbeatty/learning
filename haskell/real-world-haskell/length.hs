#!/usr/bin/env runhaskell

import System.IO

main = do print $ mylength [1,2,3]

mylength :: [a] -> Int
mylength = foldr count' 0
    where count' a b = b + 1
