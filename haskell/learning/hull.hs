
module Main (main) where

data Direction = DirectionStraight
               | DirectionLeft
               | DirectionRight
    deriving (Show)

data Vector2D = Vector2D {
    vector2DX   :: Double
  , vector2DY   :: Double
  } deriving (Show)

sq :: Double -> Double
sq x = x * x

-- abs :: Double -> Double
-- abs x
--     | x < 0     = (-x)
--     | otherwise = x

dist :: Vector2D -> Vector2D -> Double
dist a b = sqrt ((sq x2MinX1) + (sq y2MinY1))
    where x2MinX1 = abs ((vector2DX b) - (vector2DX a))
          y2MinY1 = abs ((vector2DY b) - (vector2DY a))

turn :: Vector2D -> Vector2D -> Vector2D -> Double
turn a b c = acos (((sq lenBC) + (sq lenAB) - (sq lenAC)) / bc2)
    where lenAC = dist a c
          lenAB = dist a b
          lenBC = dist b c
          bc2 = 2.0 * lenAC * lenAB

piOver2 = pi / 2.0

direction :: Vector2D -> Vector2D -> Vector2D -> Direction
direction a b c
    | t < piOver2   = DirectionLeft
    | t > piOver2   = DirectionRight
    | otherwise     = DirectionStraight
  where t = turn a b c

directionList' :: [Vector2D] -> [Direction]
directionList' (a:b:c:[]) = [direction a b c]
directionList' (a:b:c:ds) = [direction a b c] ++ (directionList' (b:c:ds))

directionList :: [Vector2D] -> Maybe [Direction]
directionList (a:b:c:[]) = Just (directionList' (a:b:c:[]))
directionList (a:b:c:ds) = Just ([direction a b c] ++ directionList' (b:c:ds))
directionList _ = Nothing

main = putStrLn "XXX"
