module Potential.Layout where

import           Potential.Core

-- This module contains some layout algorithms, which can be applied to
-- childnodes.

-- Desired behaviour for connecting nodes is given by a `layout` function
-- dependent on the source node. The `layout` function will be executed for
-- each child and determines which type a child is associated with, i.e. the
-- direction.
--
-- Example `layout` algorithm:
-- A node has to have the ability to define a `center` point by intersecting
-- two graphs described by linear equations.
--
--                   \ TopChild area  /
--                    \              /
--                     \            /
--                      +----------+
--                      |\        /|
--                      | \      / |
--                      |  \    /  |
--                      |   \  /   |
--                      |    \/    |
--   LeftChild area     |    /\    |        RightChild area
--                      |   /  \   |
--                      |  /    \  |
--                      | /      \ |
--                      |/        \|
--                      +----------+
--                     /            \
--                    /              \
--                   /BottomChild area\
--
-- The type each child has defines the start- and endside the connecting graph
-- will have. E.g. RightChild will result in a line being drawn from the right
-- side of the source to the left side of the child.

-- TODO: Clean up this function and check if the way `Layout a` is defined
-- really needs the `Child (Node a)` as a parameter.
pyramid :: (Boundable a) => Layout a
pyramid parent child
        | angleToY <= angleNE = TopChild childC
        | angleToY < angleSE = RightChild childC
        | angleToY <= angleSW = BottomChild childC
        | angleToY < angleNW = LeftChild childC
        | otherwise = TopChild childC
  where childC = contentOf child
        pc = centerOf parent
        cc = centerOf childC
        (Bounds pw ph) = boundsOf . content $ parent
        childInTranslatedCoord@(nChildX, nChildY) = subPoint cc pc
        angleToY = if nChildX >= 0
                      then angleTo Y childInTranslatedCoord
                      else 2 * pi - angleTo Y childInTranslatedCoord
        angleNE = angleTo Y (pw/2, ph/2)
        angleSE = angleTo Y (pw/2, -ph/2)
        angleSW = 2 * pi - angleTo Y (-pw/2, -ph/2)
        angleNW = 2 * pi - angleTo Y (-pw/2, ph/2)

subPoint :: Point -> Point -> Point
subPoint (x1, y1) (x2, y2) = (x1-x2, y1-y2)

centerOf :: (Boundable a) => Node a -> Point
centerOf n = (x + w/2, y - h/2)
  where (Bounds w h) = boundsOf . content $ n
        (x, y) = position n

angleTo :: Basis -> Point -> Float
angleTo X p@(x, y) = angle (1, 0) p
angleTo Y p@(x, y) = angle (0, 1) p

angle :: Point -> Point -> Float
angle p1@(x1, y1) p2@(x2, y2) = acos (dotProduct p1 p2 / (magnitude p1 * magnitude p2))

dotProduct :: Point -> Point -> Float
dotProduct (x1, y1) (x2, y2) = x1*x2 + y1*y2

magnitude :: Point -> Float
magnitude (x, y) = sqrt (x**2 + y**2)
