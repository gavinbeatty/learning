#!/usr/bin/env runhaskell

{-
http://en.wikibooks.org/wiki/Haskell/Higher-order_functions_and_Currying
-}

module Main where

import System.IO

main = do
    for 1 (<10) (+1) (print)

for :: a -> (a->Bool) -> (a->a) -> (a -> IO ()) -> IO ()
for start canCont next job
    | canCont start = do
        job start
        for (next start) canCont next job
    | otherwise = return ()

