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


romanDigit :: Char -> String
romanDigit '0' = ""
romanDigit '1' = "I"
romanDigit '2' = "II"
romanDigit '3' = "III"
romanDigit '4' = "IV"
romanDigit '5' = "V"
romanDigit '6' = "VI"
romanDigit '7' = "VII"
romanDigit '8' = "VIII"
romanDigit '9' = "IX"
romanDigit _ = ""

otherRomanDigit :: Char -> String
otherRomanDigit c
    | c == '0'      = ""
    | c == '1'      = "I"
    | c == '2'      = "II"
    | c == '3'      = "III"
    | c == '4'      = "IV"
    | c == '5'      = "V"
    | c == '6'      = "VI"
    | c == '7'      = "VII"
    | c == '8'      = "VIII"
    | c == '9'      = "IX"
    | otherwise     = ""

onSeparateLines :: [String] -> String
onSeparateLines []        = ""
onSeparateLines (x:xs)    = x ++ "\n" ++ (onSeparateLines xs)

duplicate :: [a] -> Int -> [a]
duplicate s 1       = s
duplicate s n
    | n > 1         = s ++ (duplicate s (n-1))
    | otherwise     = []

pushRight :: String -> Int -> String
pushRight s linelength = (duplicate " " (linelength - (length s))) ++ s

fibCalc :: Integer -> Integer
fibCalc 0 = 0
fibCalc 1 = 1
fibCalc n = (fibCalc (n - 1)) + (fibCalc (n - 2))

fibList :: [Integer]
fibList = 0 : 1 : [ fibCalc n | n <- [ 2, 3 .. ] ]

fibTable :: Integer -> String
fibTable 0 = "n\t\tfib n\n0\t\t" ++ (show (fibCalc 0)) ++ "\n"
fibTable n = fibTable (n-1) ++ (show n) ++ "\t\t" ++ (show (fibCalc n)) ++ "\n"


main = do print (romanDigit '4')
          print (otherRomanDigit '4')
          putStr (onSeparateLines ["Hello","There"])
          putStrLn (duplicate "Hello" 5)
          linelength <- getLine
          putStrLn (pushRight "crocodile" (read linelength :: Int))
          putStr (fibTable 10)
          print (take 40 fibList)

