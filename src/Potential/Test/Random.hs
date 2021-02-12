module Potential.Test.Random where

import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Potential.Core
import           Potential.Test.Boundable
import           System.Random.Stateful

mkNRandomNodes
  :: (StatefulGen g m) => Int -> g -> m (NonEmpty (Node ContentMock))
mkNRandomNodes n rng = go rng n []
 where
  go
    :: (StatefulGen g m)
    => g
    -> Int
    -> [Node ContentMock]
    -> m (NonEmpty (Node ContentMock))
  go g 0 ns = mkRandomNode g >>= (\n -> pure $ n :| ns)
  go g i ns = mkRandomNode g >>= (\n -> go g (i - 1) (n : ns))

mkRandomNode :: (StatefulGen g m) => g -> m (Node ContentMock)
mkRandomNode rng = do
  s   <- uniformRM (1 :: Int, 64 :: Int) rng >>= (`uniformByteStringM` rng)
  pos <- mkRandomPoint rng
  return Node { content = ContentMock s, position = pos, children = [] }

mkRandomPoint :: (StatefulGen g m) => g -> m Point
mkRandomPoint rng = do
  o <- uniformRM (0 :: Float, 1 :: Float) rng
  x <- mkRandomInt rng
  y <- mkRandomInt rng
  return (o * fromIntegral x, o * fromIntegral y)

mkRandomInt :: (StatefulGen g m) => g -> m Int
mkRandomInt = uniformM
