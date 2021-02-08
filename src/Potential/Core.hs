{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Potential.Core where

-- potential is a view agnostic graphical notebook.

-- Boundable describes a boundable type `t`, meaning that width and height can
-- be calculated to draw a box around the visual representation of `t`.
class Boundable a where
  boundsOf :: a -> Bounds

-- Bounds describe the `width` and `height` one can use to calculate a bounding
-- box from.
data Bounds = Bounds Float Float

-- Node describes a node containing `Boundable a`, the current position and a
-- list of childnodes.
data Node a where
  Node ::(Boundable a) => { content :: a
                          , position :: Point
                          , children :: [Children (Node a)]
                          } -> Node a

deriving instance Show a => Show (Node a)

data Children a = LeftChild a | RightChild a | TopChild a | BottomChild a

instance Eq (Children a) where
  (LeftChild   _) == (LeftChild   _) = True
  (RightChild  _) == (RightChild  _) = True
  (TopChild    _) == (TopChild    _) = True
  (BottomChild _) == (BottomChild _) = True
  _               == _               = False

deriving instance Show a => Show (Children a)

-- Point describes a point (x,y) on an x-y-plane.
type Point = (Float, Float)

-- Path is a list of points.
type Path = [Point]

-- Layout describes a layout function: Given the source `Node` and a target
-- `Children` containing a `Node`, return the type of `Children` the target is.
type Layout a = Node a -> Children (Node a) -> Children (Node a)

-- layoutChildren lays out the children of the given Node by applying the given
-- `Layout` algorithm to each of them and returns them in an updated Node.
layoutChildren :: (Boundable a) => Node a -> Layout a -> Node a
layoutChildren source layout = source { children = updatedChildren }
  where updatedChildren = map (layout source) . children $ source

-- leftAnchor returns the left anchor point of the given `Node`.
leftAnchor :: (Boundable a) => Node a -> Point
leftAnchor n = (x, y + (h / 2))
 where
  (Bounds _ h) = boundsOf . content $ n
  (x, y)       = position n

-- rightAnchor returns the right anchor point of the given `Node`.
rightAnchor :: (Boundable a) => Node a -> Point
rightAnchor n = (x + w, y + (h / 2))
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
bottomAnchor n = (x + (w / 2), y + h)
 where
  (Bounds w h) = boundsOf . content $ n
  (x, y)       = position n

-- connect connects the given `Node` with the given `Children (Node)` by
-- returning a list of points describing a path.
connect :: (Boundable a) => Node a -> Children (Node a) -> Path
connect source (LeftChild   target) = undefined
connect source (RightChild  target) = undefined
connect source (TopChild    target) = undefined
connect source (BottomChild target) = undefined
