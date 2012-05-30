#!/usr/bin/env runhaskell

{-
http://code.google.com/codejam/contest/351101/dashboard#s=p1
-}

module Main where

import System.IO
import System.Environment

main :: IO ()
main = do
    (f:_) <- getArgs
    withFile f ReadMode solve

caseStrings :: [String]
caseStrings = ["Case #" ++ (show n) ++ ": " | n <- [1..]]

solve :: Handle -> IO ()
solve h = do
    nstr <- hGetLine h
    let n = read nstr :: Integer
    solve' n caseStrings h

solve' :: Integer -> [String] -> Handle -> IO ()
solve' 0 _ _ = return ()
solve' _ [] _ = error "WTF"
solve' n (x:xs) h = do
    myline <- hGetLine h
    putStrLn (x ++ reverseWordsLine myline)
    solve' (n-1) xs h

reverseWordsLine :: String -> String
reverseWordsLine = unwords . reverse . words
