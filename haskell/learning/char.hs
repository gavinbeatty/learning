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


import Data.Char(ord,chr)

betweenGoodRange :: Int -> Int -> Int -> Bool
betweenGoodRange x a b
    | x >= a && x <= b      = True
    | otherwise             = False

betweenBadRange :: Int -> Int -> Int -> Bool
betweenBadRange x a b
    | x >= b && x <= a      = True
    | otherwise             = False

between :: Int -> Int -> Int -> Bool
between x a b
    | a <= b                = betweenGoodRange x a b
    | otherwise             = betweenBadRange x a b

offset :: Int
offset = (ord 'A') - (ord 'a')

isUpper :: Char -> Bool
isUpper a = between (ord a) (ord 'A') (ord 'Z')

isLower :: Char -> Bool
isLower a = between (ord a) (ord 'a') (ord 'z')

isDigit :: Char -> Bool
isDigit a = between (ord a) (ord '0') (ord '9')

dumbCharToUpper :: Char -> Char
dumbCharToUpper a = (chr ((ord a) + offset))

charToUpper :: Char -> Char
charToUpper a
    | isLower a     = dumbCharToUpper a
    | otherwise     = a

charToInt :: Char -> Int
charToInt a
    | isDigit a     = (ord a) - (ord '0')
    | otherwise     = 0

main = do print (between 1 0 5)
          print (between 1 5 0)
          print (between 1 10 6)
          print (between 1 6 10)
          print (charToUpper 'a')
          print (charToUpper '0')
          print (charToUpper 'A')
          print (charToInt '9')
          print (charToInt 'a')

