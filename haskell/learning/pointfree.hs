#!/usr/bin/env runhaskell

{-
/***************************************************************************
 *   Copyright (C) 2007 by Gavin Beatty                                    *
 *   gavinbeatty@gmail.com                                                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
-}


import qualified Data.Char

func1 :: Int -> [Int] -> [Int]
func1 x l = map (\y -> y*x) l

func2 :: (b -> Bool) -> (a -> b) -> [a] -> [b]
func2 f g l = filter f (map g l)

func3 :: (a -> a) -> [a] -> [a]
func3 f l = l ++ map f l

func4 :: [Int] -> [Int]
func4 l = map (\y -> y+2)
              (filter (\z -> z `elem` [1 .. 10])
                      (5:l))

func5 :: ((Int,Int) -> Int) -> [Int] -> Int
func5 f l = foldr (\x y -> f (y,x)) 0 l


func1pf x = map (*x)
func2pf f g = (filter f) . (map g)
func3pf f l = "Can't make \"func3 = f l = l ++ map f l\" point-free style"
func4pf = map (+2) . filter (\z -> z `elem` [1 .. 10]) . (5:)
func5pf f = foldr (flip $ curry f) 0

main = do putStrLn $ show $ func1 2 [1 .. 4]
          putStrLn $ show $ func1pf 2 [1 .. 4]
          putStrLn $ show $ func2 Data.Char.isAlpha Data.Char.toUpper "Hello 1984!"
          putStrLn $ show $ func2pf Data.Char.isAlpha Data.Char.toUpper "Hello 1984!"
          putStrLn $ show $ func3 Data.Char.toLower "Yoz Ma Ni**as!"
          putStrLn $ show $ func3pf Data.Char.toLower "Yoz Ma Ni**as!"
          putStrLn $ show $ func4 [3 .. 20]
          putStrLn $ show $ func4pf [3 .. 20]
--           putStrLn $ show $ func5 curry [1 .. 10]
--           putStrLn $ show $ func5pf curry [1 .. 10]
