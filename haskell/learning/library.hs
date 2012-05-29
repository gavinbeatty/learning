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


type Person = String
type Book   = String
type Database = [(Person,Book)]

books :: Database -> Person -> [Book]
borrowers :: Database -> Book -> [Person]
borrowed :: Database -> Book -> Bool
numBorrowed :: Database -> Person -> Int

books dbase person = [ b | (p,b) <- dbase , p == person ]
borrowers dbase book = [ p | (p,b) <- dbase , b == book ]
borrowed dbase book = ((borrowers dbase book) /= [])
numBorrowed dbase person = (length (books dbase person))

makeLoan, returnLoan :: Database -> Person -> Book -> Database

isLoanAlreadyMade :: Database -> Person -> Book -> Bool
isLoanAlreadyMade dbase person book = ([ pair | pair <- dbase , (person,book) == pair ] /= [])

makeLoan dbase person book
    | isLoanAlreadyMade dbase person book           = dbase
    | otherwise                                     = [(person,book)] ++ dbase
returnLoan dbase person book = [ pair | pair <- dbase , pair /= (person,book) ]

exbase :: Database
exbase = [("Sheila", "1"), ("Sheila", "4"), ("Mike", "1")]

main = do print (books exbase "Sheila")-- "== [1,4]"
          print (books exbase "Mike")-- "== [1]"
          print (borrowers exbase "1")-- "== [Sheila,Mike]"
          print (borrowed exbase "2")-- "== False"
          print (borrowed exbase "1")-- "== True"
          print (numBorrowed exbase "Sheila")-- "== 2"
          print (numBorrowed exbase "Mike")-- "== 1"

