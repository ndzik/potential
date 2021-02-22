{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Potential.Core where

import           Data.List.NonEmpty as NE (NonEmpty (..))

-- potential is a view agnostic graphical notebook.

-- Boundable describes a boundable type `t`, meaning that width and height can
-- be calculated to draw a box around the visual representation of `t`.
class Boundable a where
  boundsOf :: a -> Bounds

-- Bounds describe the `width` and `height` one can use to calculate a bounding
-- box from.
data Bounds = Bounds Float Float deriving (Show)

-- Node describes a node containing `Boundable a`, the current position and a
-- list of childnodes.
data Node a where
  Node ::(Boundable a) => { content :: a
                          , position :: Point
                          , children :: [Child (Node a)]
                          } -> Node a

deriving instance Show a => Show (Node a)

data Child a = LeftChild a | RightChild a | TopChild a | BottomChild a

instance Eq (Child a) where
  (LeftChild   _) == (LeftChild   _) = True
  (RightChild  _) == (RightChild  _) = True
  (TopChild    _) == (TopChild    _) = True
  (BottomChild _) == (BottomChild _) = True
  _               == _               = False

instance Functor Child where
  fmap f (LeftChild a)   = LeftChild $ f a
  fmap f (RightChild a)  = RightChild $ f a
  fmap f (TopChild a)    = TopChild $ f a
  fmap f (BottomChild a) = BottomChild $ f a

deriving instance Show a => Show (Child a)

-- Basis describes the basisvectors for a cartesian coordinate system.
data Basis = X | Y

-- Point describes a point (x,y) on an x-y-plane.
type Point = (Float, Float)

-- Path is a list of points.
type Path = NonEmpty Point

-- Layout describes a layout function: Given the source `Node` and a target
-- `Child` containing a `Node`, return the type of `Child` the target is.
type Layout a = Node a -> Child (Node a) -> Child (Node a)

-- layoutChildren lays out the children of the given Node by applying the given
-- `Layout` algorithm to each of them and returns them in an updated Node.
layoutChildren :: (Boundable a) => Node a -> Layout a -> Node a
layoutChildren source layout = source { children = updatedChildren }
  where updatedChildren = map (layout source) childs
        childs = children source

-- leftAnchor returns the left anchor point of the given `Node`.
leftAnchor :: (Boundable a) => Node a -> Point
leftAnchor n = (x, y - (h / 2))
 where
  (Bounds _ h) = boundsOf . content $ n
  (x, y)       = position n

-- rightAnchor returns the right anchor point of the given `Node`.
rightAnchor :: (Boundable a) => Node a -> Point
rightAnchor n = (x + w, y - (h / 2))
 where
  (Bounds w h) = boundsOf . content $ n
  (x, y)       = position n

-- topAnchor returns the top anchor point of the given `Node`.
topAnchor :: (Boundable a) => Node a -> Point
topAnchor n = (x + (w / 2), y)
 where
  (Bounds w h) = boundsOf . content $ n
  (x, y)       = position n

-- bottomAnchor returns the bottom anchor point of the given `Node`.
bottomAnchor :: (Boundable a) => Node a -> Point
bottomAnchor n = (x + (w / 2), y - h)
 where
  (Bounds w h) = boundsOf . content $ n
  (x, y)       = position n

-- connect connects the given `Node` with the given `Child (Node)` by
-- returning a list of points describing a path.
connect :: (Boundable a) => Node a -> Child (Node a) -> Path
connect source t@(LeftChild target) = foldr1 (<>) [leftAnchor source :| []
                                                , joints (leftAnchor source) (rightAnchor <$> t)
                                                , rightAnchor target :| []]
connect source t@(RightChild target) = foldr1 (<>) [rightAnchor source :| []
                                                 , joints (rightAnchor source) (leftAnchor <$> t)
                                                 , leftAnchor target :| []]
connect source t@(TopChild target) = foldr1 (<>) [topAnchor source :| []
                                               , joints (topAnchor source) (bottomAnchor <$> t)
                                               , bottomAnchor target :| []]
connect source t@(BottomChild target) = foldr1 (<>) [bottomAnchor source :| []
                                                  , joints (bottomAnchor source) (topAnchor <$> t)
                                                  , topAnchor target :| []]

-- joints calculates the joint points needed to draw a `zig-zag` path from a
-- source `Point` to a target `Child Point`.
joints :: Point -> Child Point -> Path
joints p1@(x1, y1) (LeftChild p2@(x2, y2)) = (newX, y1) :| [(newX, y2)]
  where newX = new X p1 p2
joints p1@(x1, y1) (RightChild p2@(x2, y2)) = (newX, y1) :| [(newX, y2)]
  where newX = new X p2 p1
joints p1@(x1, y1) (TopChild p2@(x2, y2)) = (x1, newY) :| [(x2, newY)]
  where newY = new Y p2 p1
joints p1@(x1, y1) (BottomChild p2@(x2, y2)) = (x1, newY) :| [(x2, newY)]
  where newY = new Y p2 p1

-- new returns the new Value for the given basis vector in cartesian
-- standard coordinates.
new :: Basis -> Point -> Point -> Float
new X (x1, _ ) (x2, _ ) = dist x1 x2 / 2
new Y (_ , y1) (_ , y2) = dist y1 y2 / 2

-- dist returns the absolute difference between two values. It is assumed that
-- 'a' >= 'b' holds.
dist :: Num a => a -> a -> a
dist a b = a - b

-- FIXME: Bad design?
contentOf :: Child a -> a
contentOf (LeftChild a)   = a
contentOf (RightChild a)  = a
contentOf (TopChild a)    = a
contentOf (BottomChild a) = a
