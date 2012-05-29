#!/usr/bin/env runhaskell
module Main where

main :: IO ()
main = do print $ intersperse ',' ""
          print $ intersperse ',' "1"
          print $ intersperse ',' "123"

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse i (x:xs) = x:i:intersperse i xs
