#!/usr/bin/env runhaskell

{- /***************************************************************************
 -  *   Copyright (C) 2007 by Gavin Beatty                                    *
 -  *   gavinbeatty@gmail.com                                                 *
 -  *                                                                         *
 -  *   This program is free software; you can redistribute it and/or modify  *
 -  *   it under the terms of the GNU General Public License as published by  *
 -  *   the Free Software Foundation; either version 2 of the License, or     *
 -  *   (at your option) any later version.                                   *
 -  *                                                                         *
 -  *   This program is distributed in the hope that it will be useful,       *
 -  *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 -  *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 -  *   GNU General Public License for more details.                          *
 -  *                                                                         *
 -  *   You should have received a copy of the GNU General Public License     *
 -  *   along with this program; if not, write to the                         *
 -  *   Free Software Foundation, Inc.,                                       *
 -  *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 -  ***************************************************************************/ -}

import qualified Data.Char

inflist :: [Int]
inflist = [2, 2 .. 2]

isEven :: Int -> Bool
isEven n = (n `mod` 2 == 0)

somelist :: [Int]
somelist = [2, 3 .. 10]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ m+n | (m,n) <- pairList ]

thepairlist :: [(Int,Int)]
thepairlist = [(1,2), (4,5), (6,10), (1,10)]

-- | Returns a String of only the letters in the input, converted to uppercase
capitalizeLetters :: String     -- ^ The arbitrary String input
                  -> String     -- ^ The isAlpha Char elements, converted to uppercase
capitalizeLetters s = [ Data.Char.toUpper el | el <- s , Data.Char.isAlpha el ]


-- | Returns the divisors of a positive Int
divisors :: Int     -- ^ A positive Int
         -> [Int]   -- ^ A list of divisors of the input Int (empty list if no divisors)
divisors n = [ a | a <- [ 1 .. n ] , n `mod` a == 0 ]

isPrime :: Int -> Bool
isPrime n = ((divisors n) == [1,n])

matches :: Int -> [Int] -> [Int]
matches n list = [ a | a <- list , a == n ]

elementof :: Int -> [Int] -> Bool
elementof n list = (matches n list /= [])

main = do putStrLn "somelist:"
          print somelist
          putStrLn "Evenness elements of somelist:"
          print [isEven n | n <- somelist]
          putStrLn "2*n such that n is in somelist, is even, and is less than or equal to 5:"
          print [2*n | n <- somelist, isEven n, n <= 5]
          putStrLn "pairList:"
          print thepairlist
          putStrLn "addPairs pairList:"
          print (addPairs thepairlist)
          print (capitalizeLetters "abcdefg1234567ABCDEFGisASentence")
          print (divisors 5)
          print (isPrime 2000)
          print (isPrime 1979)
          print (matches 3 [33, 40, 3, 4, 10, 3])
          print (elementof 3 [33, 40, 3, 4, 10, 3])
          print (elementof 6 [33, 40, 3, 4, 10, 3])

