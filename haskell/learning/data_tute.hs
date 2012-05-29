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


data Triple a b c = Triple a b c

tripFst :: ( Triple a b c ) -> a
tripFst ( Triple a b c ) = a

tripSnd :: ( Triple a b c ) -> b
tripSnd ( Triple a b c ) = b

tripThrd :: ( Triple a b c ) -> c
tripThrd ( Triple a b c ) = c

-- | Takes 4 elements, the first two elements must be of the same type and similarly for the last two elements
data Quadruple a b = Quadruple a a b b

quadFirstTwo :: Quadruple a b -> [a]
quadFirstTwo (Quadruple x y z t) = [x,y]

quadLastTwo :: Quadruple a b -> [b]
quadLastTwo (Quadruple x y z t) = [z,t]

-- | Example of different Constructors
data Tuple a b c d = Tuple1 a
                   | Tuple2 a b
                   | Tuple3 a b c
                   | Tuple4 a b c d

tuple1 :: Tuple a b c d -> Maybe a
tuple1 (Tuple4 x y z t) = Just x
tuple1 (Tuple3 x y z) = Just x
tuple1 (Tuple2 x y) = Just x
tuple1 (Tuple1 x) = Just x

tuple2 :: Tuple a b c d -> Maybe b
tuple2 (Tuple4 x y z t) = Just y
tuple2 (Tuple3 x y z) = Just y
tuple2 (Tuple2 x y) = Just y
tuple2 (Tuple1 _) = Nothing

tuple3 :: Tuple a b c d -> Maybe c
tuple3 (Tuple4 x y z t) = Just z
tuple3 (Tuple3 x y z) = Just z
tuple3 (Tuple2 _ _) = Nothing
tuple3 (Tuple1 _) = Nothing

tuple4 :: Tuple a b c d -> Maybe d
tuple4 (Tuple4 x y z t) = Just t
tuple4 (Tuple3 _ _ _) = Nothing
tuple4 (Tuple2 _ _) = Nothing
tuple4 (Tuple1 _) = Nothing

fromMyTuple :: Tuple a b c d -> Either (Either a (a,b)) (Either (a,b,c) (a,b,c,d))
fromMyTuple (Tuple4 x y z t) = Right (Right (x,y,z,t))
fromMyTuple (Tuple3 x y z) = Right (Left (x,y,z))
fromMyTuple (Tuple2 x y) = Left (Right (x,y))
fromMyTuple (Tuple1 x) = Left (Left x)

data List a = Nil
            | Cons a (List a)

listHead :: (List a) -> a
listHead (Cons x xs) = x

listTail :: (List a) -> (List a)
listTail (Cons x xs) = xs

listFoldl :: (a -> b -> a) -> a -> (List b) -> a
listFoldl f x Nil = x
listFoldl f x (Cons y ys) = listFoldl f (f x y) ys

listFoldr :: (b -> a -> a) -> a -> (List b) -> a
listFoldr f x Nil = x
listFoldr f x (Cons y ys) = f y (listFoldr f x ys)

data BinaryTree a = Leaf a
                  | Branch (BinaryTree a) a (BinaryTree a)

elements :: (BinaryTree a) -> [a]
elements (Leaf a)       = [a]
elements (Branch l v r) = elements l ++ [v] ++ elements r

-- btreeFoldr ::
btreeFoldr f x (Leaf a)       = f x a
btreeFoldr f x (Branch l v r) = btreeFoldr f (f x (btreeFoldr f v r)) l

-- elements2 :: (BinaryTree a) -> [a]
elements2 = btreeFoldr (++) []

main = do putStrLn (show (tripFst (Triple "a" 'b' 3)))
          putStrLn (show (quadFirstTwo (Quadruple "a" "b" 1 2)))
          putStrLn (show (quadLastTwo (Quadruple "a" "b" 4 2)))
          putStrLn (show (tuple4 (Tuple4 1 1.3 'a' "hello")))
--           putStrLn (show Nothing)
{-
          if theresult == Nothing
             then putStrLn "Nothing"
             else putStrLn (show theresult)
                  where theresult = tuple4 (Tuple3 1 1.2 'a')
-}
          let i = (fromMyTuple (Tuple1 4))
--           let s = (show i)
          putStrLn (show (fromMyTuple (Tuple4 1 1.3 'a' "hello")))

