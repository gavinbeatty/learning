#!/usr/bin/env runhaskell

{-
http://code.google.com/codejam/contest/351101/dashboard#s=p0
-}

module Main where

import System.IO
import System.Environment

main :: IO ()
main = do
    (f:_) <- getArgs
    withFile f ReadMode handleFile

caseStrings :: [String]
caseStrings = ["Case #" ++ show n ++ ": " | n <- [1..]]

handleFile :: Handle -> IO ()
handleFile h = do
    line <- hGetLine h
    let n = read line :: Integer
    readAndPutCases caseStrings n h

readAndPutCases :: [String] -> Integer -> Handle -> IO ()
readAndPutCases prefixes count h
    | count > 0 = do
        (target,values) <- readCase h
        let ans = findAns target values
        case ans of
            Just x -> putAns prefixes x
            Nothing -> putStrLn ("No answer for target(" ++ (show target) ++ ") values" ++ (show values))
        readAndPutCases (tail prefixes) (count-1) h
    | count <= 0 = do return ()

putAns :: [String] -> (Integer,Integer) -> IO ()
putAns (prefix:xs) (a,b) = do
    putStrLn (prefix ++ (show a) ++ " " ++ (show b))

readCase :: Handle -> IO (Integer,[Integer])
readCase h = do
    targetio <- hGetLine h
    let target = read targetio :: Integer
    hGetLine h -- skip the number of entries in a line
    valuesstr <- hGetLine h
    return (target, [read x :: Integer | x <- words valuesstr])

findAns :: Integer -> [Integer] -> Maybe (Integer,Integer)
findAns target values = findAns' target $ zip [1..] values

findAns' :: Integer -> [(Integer,Integer)] -> Maybe (Integer,Integer)
findAns' _ [] = Nothing
findAns' _ [x] = Nothing
findAns' target (x:xs) =
    case (findAns'' target x xs) of
        Just foo -> Just foo
        Nothing -> findAns' target xs

findAns'' :: Integer -> (Integer,Integer) -> [(Integer,Integer)] -> Maybe (Integer,Integer)
findAns'' target pick [] = Nothing
findAns'' target pick (x:xs)
    | target == (snd pick) + (snd x)
        = Just (fst pick, fst x)
    | otherwise
        = findAns'' target pick xs

