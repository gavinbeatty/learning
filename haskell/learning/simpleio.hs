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


import IO

main = do putStrLn "Enter your name:"
          name <- getLine
          putStrLn
              (if name `elem` ["Simon", "John", "Phil"]
                  then "Haskell is a great programming language, I say!"
                  else if name == "Koen"
                          then "Debugging Haskell sure is fun!"
                          else "I don't know who you are..."
              )
