#!/usr/bin/env runhaskell
module Main where

data Tree a = Node a (Tree a) (Tree a)
            | Empty

height :: Tree a -> Int
height Empty = 0
height (Node _ t1 t2) = 1 + (max (height t1) (height t2))

main :: IO ()
main = do print $ height (Node 1 Empty Empty)
          print $ height (Node 1 (Node 2 Empty Empty) Empty)
          print $ height (Node 1 (Node 2 Empty (Node 3 Empty Empty)) (Node 4 Empty Empty))
          print $ height (Node 1 (Node 2 Empty (Node 3 (Node 5 Empty Empty) Empty)) (Node 4 Empty Empty))
