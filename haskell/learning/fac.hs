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


import qualified IO

fac :: Integer -> Integer
fac 0 = 1
fac n
    | n > 0     = n * fac(n-1)
    | otherwise = -1

main :: IO ()
main = do IO.hSetBuffering IO.stdin IO.LineBuffering
          putStrLn "Enter a natural number:"
          i <- getLine
          putStrLn (show (fac (read i :: Integer)))
