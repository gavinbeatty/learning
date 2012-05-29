#!/usr/bin/env runhaskell
module Main where

data Point = Cartesian Double Double

toList :: Point -> [Double]
toList (Cartesian x y) = [x, y]

main :: IO ()
main = do print $ direction (Cartesian 0 0) (Cartesian 0 1) (Cartesian 0 2)  -- Straight
          print $ direction (Cartesian 0 0) (Cartesian 1 0) (Cartesian 2 0)  -- Straight
          print $ direction (Cartesian 0 0) (Cartesian 1 0) (Cartesian 0 1)  -- TurnRight
          print $ direction (Cartesian 0 0) (Cartesian 0 1) (Cartesian 1 1)  -- TurnLeft
          print $ direction (Cartesian 0 0) (Cartesian 1 0) (Cartesian (-1) (-1))  -- TurnRight
          print $ direction (Cartesian 0 0) (Cartesian 1 0) (Cartesian (-1) 2)  -- TurnRight

data Direction = TurnLeft | TurnRight | Straight
instance Show Direction where
    show TurnLeft = "TurnLeft"
    show TurnRight = "TurnRight"
    show Straight = "Straight"

slope :: Point -> Point -> Maybe Double
slope (Cartesian x1 y1) (Cartesian x2 y2) = go (y2 - y1) (x2 - x1)
    where go num denom
            | denom == 0 = Nothing
            | otherwise  = Just (num / denom)

angle :: Point -> Point -> Double
angle (Cartesian x1 y1) (Cartesian x2 y2) = angNorm $ (atan2 y2 x2) - (atan2 y1 x1)
    where angNorm x
            | x < -pi   = angNorm $ x + pi
            | x > pi    = angNorm $ x - pi
            | otherwise = x

directionViaAngle :: Point -> Point -> Point -> Direction
directionViaAngle a@(Cartesian x1 y1) b@(Cartesian ox oy) c@(Cartesian x2 y2) =
    if (slope a b) == (slope b c) then Straight
    else go (Cartesian (x1-ox) (y1-oy)) (Cartesian (x2-ox) (y2-oy))
      where go x y
              | ang == 0 = Straight
              | ang < 0 = TurnRight
              | otherwise = TurnLeft
                where ang = angle x y

directionViaCross :: Point -> Point -> Point -> Direction
directionViaCross = undefined

direction = directionViaAngle
--direction = directionViaCross
