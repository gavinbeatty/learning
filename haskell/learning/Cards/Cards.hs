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


module Cards ( Card(..),
               Suit(..),
               Face(..),
               strCard,
               mkCard
             )
    where

data Suit = Clubs | Hearts | Spades | Diamonds
data Face = King | Queen | Jack | Ace | Number Int

data Card = Card Suit Face


mkCard :: Suit -> Face -> Maybe Card
mkCard s f = case f of
                     Number i -> case (compare i 0) of
                                         GT -> case (compare i 11) of
                                                       LT -> Just (Card s f)
                                                       _  -> Nothing
                                         _  -> Nothing
                     _        -> Just (Card s f)


strSuit :: Suit -> String
strSuit s = case s of
                 Clubs    -> "Clubs"
                 Hearts   -> "Hearts"
                 Spades   -> "Spades"
                 Diamonds -> "Diamonds"
--                  _        -> "Error"

strFace :: Face -> String
strFace f = case f of
                 King     -> "King"
                 Queen    -> "Queen"
                 Jack     -> "Jack"
                 Number i -> (show i)
--                  _        -> "Error"

strCard :: Card -> String
strCard (Card s f) = "Suit: " ++ (strSuit s) ++ "; Face: " ++ (strFace f)


