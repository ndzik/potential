{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import           Control.Monad
import           Data.Functor                   ( (<&>) )
import           Data.Time.Clock.POSIX
import           Potential.Core
import           Potential.Test.Boundable
import           Potential.Test.Random
import           System.Random.Stateful
import           Test.Hspec

main :: IO ()
main = do
  rng <- getPOSIXTime <&> round <&> mkStdGen
  hspec $ describe "Node" $ do
    context "when laying out children" $ do
      it "lays them out according to RightChilds" (layoutTestCase rng RightChild)
      it "lays them out according to LeftChilds" (layoutTestCase rng LeftChild)
      it "lays them out according to TopChilds" (layoutTestCase rng TopChild)
      it "lays them out according to BottomChilds" (layoutTestCase rng BottomChild)
  hspec $ describe "Connection" $ do
    context "when given a source and target" $ do
      it "correctly chooses the sides of the bounding box" $ do
        let n = runStateGen_ rng mkRandomNode in connectionTestCase n (LeftChild n)

connectionTestCase :: Node ContentMock -> Children (Node ContentMock) -> IO ()
connectionTestCase source tgt@(LeftChild target) = connect source tgt `shouldContain` [leftAnchor source, rightAnchor target]
connectionTestCase source tgt@(RightChild target) = connect source tgt `shouldContain` [rightAnchor source, leftAnchor target]
connectionTestCase source tgt@(TopChild target) = connect source tgt `shouldContain` [topAnchor source, bottomAnchor target]
connectionTestCase source tgt@(BottomChild target) = connect source tgt `shouldContain` [bottomAnchor source, topAnchor target]

layoutTestCase :: (RandomGen g) => g -> (Node ContentMock -> Children (Node ContentMock)) -> IO ()
layoutTestCase rng childCtor = do
  let (x:xs) = runStateGen_ rng $ mkNRandomNodes 100
      n = x {children = map LeftChild xs}
      layedoutChildren = children $ layoutChildren n (testLayoutFn childCtor)
   in mapM_ (`shouldSatisfy` (childCtor n ==)) layedoutChildren

testLayoutFn :: (Boundable a) => (Node a -> Children (Node a)) -> Node a -> Children (Node a) -> Children (Node a)
testLayoutFn ctor _ (LeftChild n) = ctor n
testLayoutFn ctor _ (RightChild n) = ctor n
testLayoutFn ctor _ (TopChild n) = ctor n
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
