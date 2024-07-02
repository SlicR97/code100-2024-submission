module Lib (solve) where

data Point = Point { pX :: Int, pY :: Int }

data Shape
    = Rect { rTopLeft :: Point, rBottomRight :: Point }
    | Donut { dCenter :: Point, dInnerRadius :: Int, dOuterRadius :: Int }

type Shapes = [Shape]

imageShapes :: Shapes
imageShapes =
    [ Rect { rTopLeft = Point { pX = 145, pY = 75 }, rBottomRight = Point { pX = 165, pY = 225 } }
    , Donut { dCenter = Point { pX = 260, pY = 150 }, dInnerRadius = 55, dOuterRadius = 75 }
    , Donut { dCenter = Point { pX = 410, pY = 150 }, dInnerRadius = 55, dOuterRadius = 75 }
    ]

vector :: Point -> Point -> Float
vector (Point x1 y1) (Point x2 y2) = sqrt . fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

isInShape :: Point -> Shape -> Bool
isInShape p (Rect topLeft bottomRight) =
    let inX = pX topLeft <= pX p && pX bottomRight >= pX p
        inY = pY topLeft <= pY p && pY bottomRight >= pY p
    in inX && inY
isInShape p (Donut center innerRadius outerRadius) =
    let v = vector p center
    in v >= fromIntegral innerRadius && v <= fromIntegral outerRadius

isInShapes :: Point -> Shapes -> Bool
isInShapes p = any (isInShape p)

pointFromPair :: (Int, Int) -> Point
pointFromPair (x, y) = Point { pX = x, pY = y }

pointToPair :: Point -> (Int, Int)
pointToPair (Point x y) = (x, y)

solve :: [(Int, Int)] -> [(Int, Int)]
solve = map pointToPair . filter (`isInShapes` imageShapes) . map pointFromPair
