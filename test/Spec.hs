{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Control.Monad.ST
import           Data.Functor             ((<&>))
import           Data.List.NonEmpty       as NE (head, tail, toList)
import           Data.Time.Clock.POSIX
import           Potential.Core
import           Potential.Test.Boundable
import           Potential.Test.Random
import           System.Random.Stateful
import           Test.Hspec

-- TODO: Implement error gatherer directive to allow accumulating multiple
-- failed assertions and display them after the testrunner is done.

main :: IO ()
main = do
  rng <- getPOSIXTime >>= (newIOGenM . mkStdGen) . round
  hspec $ describe "Node" $ do
    context "when laying out children" $ do
      it "lays them out according to RightChilds"
         (layoutTestCase rng RightChild)
      it "lays them out according to LeftChilds" (layoutTestCase rng LeftChild)
      it "lays them out according to TopChilds"  (layoutTestCase rng TopChild)
      it "lays them out according to BottomChilds"
         (layoutTestCase rng BottomChild)
  hspec $ describe "Connection" $ do
    context "when given a source and target" $ do
      it "correctly chooses the sides of the bounding box" $ do
        connectionTestCase rng LeftChild
        connectionTestCase rng RightChild
        connectionTestCase rng TopChild
        connectionTestCase rng BottomChild

connectionTestCase :: StatefulGen g IO => g -> (Node ContentMock -> Children (Node ContentMock)) -> IO ()
connectionTestCase rng ctor = do
  source <- mkRandomNode rng
  target <- mkRandomNode rng
  let path = connect source (ctor target)
  isZigZag path `shouldBe` True
  testAnchor source (ctor target) path

testAnchor :: Node ContentMock -> Children (Node ContentMock) -> Path -> IO ()
testAnchor source (LeftChild target) path = sequence_
  [ toList path `shouldStartWith` [leftAnchor source]
  , toList path `shouldEndWith` [rightAnchor target]
  ]
testAnchor source (RightChild target) path = sequence_
  [ toList path `shouldStartWith` [rightAnchor source]
  , toList path `shouldEndWith` [leftAnchor target]
  ]
testAnchor source (TopChild target) path = sequence_
  [ toList path `shouldStartWith` [topAnchor source]
  , toList path `shouldEndWith` [bottomAnchor target]
  ]
testAnchor source (BottomChild target) path = sequence_
  [ toList path `shouldStartWith` [bottomAnchor source]
  , toList path `shouldEndWith` [topAnchor target]
  ]

isZigZag :: Path -> Bool
isZigZag ps = go (NE.head ps) (NE.tail ps)
 where
  go _ [] = True
  go (x1, y1) (nextPoint@(x2, y2) : rest) =
    (x1 == x2 || y1 == y2) && go nextPoint rest

layoutTestCase
  :: (StatefulGen g IO)
  => g
  -> (Node ContentMock -> Children (Node ContentMock))
  -> IO ()
layoutTestCase rng childCtor = do
  xs <- mkNRandomNodes 100 rng
  let x = NE.head xs
      n = x { children = map LeftChild (NE.tail xs) }
      layedoutChildren = children $ layoutChildren n (testLayoutFn childCtor)
   in mapM_ (`shouldSatisfy` (childCtor n ==)) layedoutChildren

testLayoutFn
  :: (Boundable a)
  => (Node a -> Children (Node a))
  -> Node a
  -> Children (Node a)
  -> Children (Node a)
testLayoutFn ctor _ (LeftChild   n) = ctor n
testLayoutFn ctor _ (RightChild  n) = ctor n
testLayoutFn ctor _ (TopChild    n) = ctor n
testLayoutFn ctor _ (BottomChild n) = ctor n

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
--  The type each child has defines the start- and endside the connecting graph
--  will have. E.g. RightChild will result in a line being drawn from the right
--  side of the source to the left side of the child.
