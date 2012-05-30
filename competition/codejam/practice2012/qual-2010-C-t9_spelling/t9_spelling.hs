#!/usr/bin/env runhaskell

{-
http://code.google.com/codejam/contest/351101/dashboard#s=p2
-}

module Main where

import System.IO
import System.Environment
import Data.Char
import Data.Maybe

main = do
    (f:_) <- getArgs
    withFile f ReadMode (solve caseStrings)

caseStrings :: [String]
caseStrings = ["Case #" ++ (show n) ++ ": " | n <- [1..]]

solve :: [String] -> Handle -> IO ()
solve ps h = do
    nstr <- hGetLine h
    solve' ps (read nstr :: Integer) h

solve' :: [String] -> Integer -> Handle -> IO ()
solve' [] _ _ = do error "WTF"
solve' _ 0 _ = return ()
solve' (p:ps) count h = do
    myline <- hGetLine h
    putStrLn (p ++ (maybe "WTF" id (t9Decode myline)))
    solve' ps (count-1) h

t9IsValid :: Char -> Bool
t9IsValid c = c == ' ' || isAsciiLower c

t9CharToString :: Char -> Maybe String
t9CharToString c
    | t9IsValid c = Just (t9CharToString' c)
    | otherwise   = Nothing

t9SameKey :: Char -> Char -> Bool
t9SameKey a b = (t9Bin' a) == (t9Bin' b)

t9Bin' :: Char -> Integer
t9Bin' c
    | c `elem` "abc"    = 2
    | c `elem` "def"    = 3
    | c `elem` "ghi"    = 4
    | c `elem` "jkl"    = 5
    | c `elem` "mno"    = 6
    | c `elem` "pqrs"   = 7
    | c `elem` "tuv"    = 8
    | c `elem` "wxyz"   = 9
    | c == ' '          = 0
    | otherwise         = -1

t9CharToString' :: Char -> String
t9CharToString' c = case c of
    'a' -> "2"
    'b' -> "22"
    'c' -> "222"
    'd' -> "3"
    'e' -> "33"
    'f' -> "333"
    'g' -> "4"
    'h' -> "44"
    'i' -> "444"
    'j' -> "5"
    'k' -> "55"
    'l' -> "555"
    'm' -> "6"
    'n' -> "66"
    'o' -> "666"
    'p' -> "7"
    'q' -> "77"
    'r' -> "777"
    's' -> "7777"
    't' -> "8"
    'u' -> "88"
    'v' -> "888"
    'w' -> "9"
    'x' -> "99"
    'y' -> "999"
    'z' -> "9999"
    ' ' -> "0"
    _   -> error "WTF"

t9Decode :: String -> Maybe String
t9Decode "" = Just ""
t9Decode [c] = t9CharToString c
t9Decode x = t9Decode' "" "" x

t9Decode' :: String -> String -> String -> Maybe String
t9Decode' accum _ "" = Just accum
t9Decode' accum sep x = do
    let (noreps,rest) = largestNoRepeats t9SameKey x
    let t9 = t9NoRepStringToString noreps
    case t9 of
        Nothing  -> Nothing
        Just t9' -> t9Decode' (accum ++ sep ++ t9') " " rest

t9NoRepStringToString :: String -> Maybe String
t9NoRepStringToString noreps = do
    let m = map t9CharToString noreps
    case any isNothing m of
        True  -> Nothing
        False -> Just (concat (catMaybes m))

largestNoRepeats :: Eq a => (a -> a -> Bool) -> [a] -> ([a],[a])
largestNoRepeats = largestNoRepeats' []

largestNoRepeats' :: Eq a => [a] -> (a -> a -> Bool) -> [a] -> ([a],[a])
largestNoRepeats' [] _ [] = ([],[])
largestNoRepeats' [] f y = largestNoRepeats' [head y] f (tail y)
largestNoRepeats' x _ [] = (x,[])
largestNoRepeats' x f y
    | (last x) `f` (head y)
        = (x,y)
    | otherwise
        = largestNoRepeats' (x ++ [head y]) f (tail y)

