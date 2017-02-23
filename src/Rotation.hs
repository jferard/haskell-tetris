module Rotation (
    rotateLeft,
    rotateRight,
    Point
) where

type Point a = (a, a)

rotateLeft :: Num a => Point a -> Point a -> Point a
rotateLeft (ox, oy) (x, y) = let (dx, dy) = (x-ox, y-oy) in (ox - dy, oy + dx)

rotateRight :: Num a => Point a -> Point a -> Point a
rotateRight (ox, oy) (x, y) = let (dx, dy) = (x-ox, y-oy) in (ox + dy, oy - dx)