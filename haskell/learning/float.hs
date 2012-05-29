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


averageThree :: Int -> Int -> Int -> Float
averageThree a b c = ((fromIntegral a) + (fromIntegral b) + (fromIntegral c)) / 3.0::Float

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c
    | (fromIntegral a) >= (averageThree a b c) && (fromIntegral b) >= (averageThree a b c) && (fromIntegral c) >= (averageThree a b c)       = 3
    | (fromIntegral b) >= (averageThree a b c) && (fromIntegral c) >= (averageThree a b c)                                                   = 2
    | (fromIntegral c) >= (averageThree a b c)                                                                                               = 1
    | otherwise                                                                                                                              = 0

numberNDRoots, numberRoots :: Float -> Float -> Float -> Int
numberNDRoots a b c
    | (b**2.0) > (4.0*a*c)      = 2
    | (b**2.0) == (4.0*a*c)     = 1
    | otherwise                 = 0

numberRoots a b c
    | a == 0.0 && b /= 0.0      = 1
    | a == 0.0 && c /= 0.0      = 0
    | a == 0.0                  = 3
    | otherwise                 = numberNDRoots a b c


smallerRoot, largerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
    | ((numberRoots a b c) == 3 || (numberRoots a b c) == 0)      = 0
    | otherwise                                                   = ((-b) - sqrt (b**2.0 - 4.0*a*c)) / (2.0 * a)
largerRoot a b c
    | ((numberRoots a b c) == 3 || (numberRoots a b c) == 0)      = 0
    | otherwise                                                   = ((-b) + sqrt (b**2.0 - 4.0*a*c)) / (2.0 * a)

main = do print (averageThree 1 2 3)
          print (howManyAboveAverage 1 2 3)
          print (numberNDRoots 2.0 4.0 2.5)
          print (numberRoots 2.0 4.0 2.5)
          print (numberNDRoots 1.0 (-4.0) 4.0)
          print (numberRoots 1.0 (-4.0) 4.0)
          print (numberNDRoots 1.0 10.0 2.0)
          print (numberRoots 1.0 10.0 2.0)

          print (numberRoots 0.0 0.0 0.0)
          print (numberRoots 0.0 10.0 0.0)
          print (numberRoots 0.0 0.0 1.0)
          print (numberRoots 0.0 0.0 1.0)

          print (smallerRoot 1.0 4.0 1.0)
          print (largerRoot 1.0 4.0 1.0)
