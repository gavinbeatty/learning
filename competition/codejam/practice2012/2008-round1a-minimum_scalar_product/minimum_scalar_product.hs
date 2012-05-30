#!/usr/bin/env runhaskell
{-# LANGUAGE BangPatterns #-}
{-
http://code.google.com/codejam/contest/32016/dashboard#s=p0
-}

module Main where

import System.IO
import System.Environment
import Data.Int
import Data.List
import Control.Monad
import qualified Control.Parallel as P

s_par :: a -> b -> b
s_par x y = y

s_pseq :: a -> b -> b
s_pseq x y = y

main :: IO ()
main = do
    (f:_) <- getArgs
    withFile f ReadMode solve

caseStrings :: [String]
caseStrings = ["Case #" ++ (show n) ++ ": " | n <- [1..]]

solve :: Handle -> IO ()
solve h = do
    nstr <- hGetLine h
    solve' h caseStrings (read nstr :: Int64)

solve' :: Handle -> [String] -> Int64 -> IO ()
solve' _ _ 0 = return ()
solve' _ [] count = error "WTF"
solve' h (p:ps) !count = do
    hGetLine h -- skip vector length
    vecLine1 <- hGetLine h
    vecLine2 <- hGetLine h
    let vec1 = [(read x :: Int64) | x <- (words vecLine1)]
    let vec2 = [(read x :: Int64) | x <- (words vecLine2)]
    checkEq (length vec1) (length vec2)
    putStrLn (p ++ (show $ findAns vec1 vec2))
    solve' h ps (count-1)

checkEq :: Int -> Int -> IO ()
checkEq x y
    | x == y    = return ()
    | otherwise = error "WTF"

findAns :: (Ord a, Num a) => [a] -> [a] -> a
findAns [] [] = error "WTF"
findAns (x:xs) [] = error "WTF"
findAns [] (y:ys) = error "WTF"
findAns !vec1 !vec2 = uniq' `P.par` minimum [ dotProd vec1 x | x <- uniq' ]
    where uniq' = uniquePermutations vec2

dotProd :: Num a => [a] -> [a] -> a
dotProd xs ys = zipped' `P.par` sum [ x*y | (x,y) <- zipped' ]
    where zipped' = zip xs ys

--scalarProd :: Num a => [a] -> [a] -> a
--scalarProd [] _ = error "WTF"
--scalarProd _ [] = error "WTF"
--scalarProd (x:xt) (y:yt) = scalarProd' (x*y) xt yt
--
--scalarProd' :: Num a => a -> [a] -> [a] -> a
--scalarProd' accum [] [] = accum
--scalarProd' accum (x:xt) (y:yt) = scalarProd' (accum+(x*y)) xt yt

merges' :: [a] -> [a] -> [[a]]
merges' [] ys = [ys]
merges' xs [] = [xs]
merges' xs@(x:xt) ys@(y:yt) = part2 `s_par` (part1 `s_pseq` (part1 ++ part2))
    where part1 = [x:zs | zs <- merges' xt ys]
          part2 = [y:zs | zs <- merges' xs yt]

uniquePermutations :: Ord a => [a] -> [[a]]
uniquePermutations = foldM merges' [] . group . sort
