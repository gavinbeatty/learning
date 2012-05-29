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

_cfold :: (b -> a -> (a -> a) -> a) -> a -> [b] -> a
_cfold f z []     = z
_cfold f z (x:xs) = f x z (\y -> _cfold f y xs)


my_map :: (a -> b) -> [a] -> [b]
my_map _ []     = []
my_map f (x:xs) = (f x) : (my_map f xs)

-- cmap _    []    _      = []
-- cmap fxn (x:xs) cfxn   = fxn x (\y z -> cmap fxn xs cfxn)

main :: IO ()
main = do putStrLn (show (my_map Data.Char.toUpper "Gavin Beatty"))
--           putStrLn (show (cmap Data.Char.toUpper "Beatty, Gavin"))

