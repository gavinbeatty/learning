#!/usr/bin/env runhaskell

module Main where

data List a = Cons a (List a)
            | Nil

instance Eq a => Eq (List a) where
    (==) = listEq
instance Show a => Show (List a) where
    show = listShow

listEq :: Eq a => (List a) -> (List a) -> Bool
listEq Nil Nil                  = True
listEq (Cons x xs) (Cons y ys)  = (x == y) && (listEq xs ys)
listEq _ _                      = False

listShow :: Show a => (List a) -> String
listShow Nil = "[]"
listShow _ = undefined
{-
listShow y = go "[" y
    where go prefix l =
      case l of
        Cons x Nil -> prefix ++ (show x) ++ "]"
        Cons x xs  -> ", " ++ (show x) ++ (go ", " xs)
-}

x1 :: List Int
x1 = Cons 1 (Cons 2 (Cons 3 Nil))
x2 :: [Int]
x2 = [1, 2, 3]

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

fromMyList :: List a -> [a]
fromMyList Nil      = []
fromMyList (Cons x xs) = x : fromMyList xs

prop_fromList2Way :: Eq a => [a] -> Bool
prop_fromList2Way xs = (xs == (fromMyList $ fromList xs))

prop_fromList :: Bool
prop_fromList = x1 == (fromList x2)
prop_fromMyList :: Bool
prop_fromMyList = x2 == (fromMyList x1)

main :: IO ()
main = do print $ prop_fromList2Way "string"
          print $ prop_fromList
          print $ prop_fromMyList
