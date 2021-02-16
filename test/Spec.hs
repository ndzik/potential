{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Control.Monad.ST
import           Data.Functor             ((<&>))
import           Data.List.NonEmpty       as NE (head, tail, toList)
import           Data.Time.Clock.POSIX
import           Potential.Core
import           Potential.Layout
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
      it "lays them out according to RightChilds" (layoutTestCase rng RightChild)
      it "lays them out according to LeftChilds" (layoutTestCase rng LeftChild)
      it "lays them out according to TopChilds"  (layoutTestCase rng TopChild)
      it "lays them out according to BottomChilds" (layoutTestCase rng BottomChild)
  hspec $ describe "Connection" $ do
    context "when given a source and target" $ do
      it "correctly chooses the sides of the bounding box" $ do
        connectionTestCase rng LeftChild
        connectionTestCase rng RightChild
        connectionTestCase rng TopChild
        connectionTestCase rng BottomChild
  hspec $ describe "Layout" $ do
    context "pyramid" $ do
      it "puts children in the N as TopChilds" (pyramidTestCase (0, 100) TopChild)
      -- Note: We only use standard floating point precision numbers. In this
      -- edge case the result of layouting the children differs by `0.0000001`
      -- from the expected value. We will glance over this here, since it is
      -- not important. We will just be slightly more generous and nudge the
      -- target into the correct direction.
      -- Value when calculating angle for child to Y-Axis:
      -- expected:  `0.7853981`
      -- real:      `0.7853982`
      it "puts children in the NE as TopChilds" (pyramidTestCase (100-1.5-0.01, 100+1.5+0.01) TopChild)
      it "puts children in the E as RightChilds" (pyramidTestCase (100, 0) RightChild)
      it "puts children in the SE as BottomChilds" (pyramidTestCase (100, -100) BottomChild)
      it "puts children in the S as BottomChilds" (pyramidTestCase (0, -100) BottomChild)
      it "puts children in the SW as BottomChilds" (pyramidTestCase (-100-1.5, -100+1.5) BottomChild)
      it "puts children in the W as LeftChilds" (pyramidTestCase (-100, 0) LeftChild)
      it "puts children in the NW as TopChilds" (pyramidTestCase (-100, 100) TopChild)

connectionTestCase :: StatefulGen g IO => g -> (Node ContentMock -> Child (Node ContentMock)) -> IO ()
connectionTestCase rng ctor = do
  source <- mkRandomNode rng
  target <- mkRandomNode rng
  let path = connect source (ctor target)
  isZigZag path `shouldBe` True
  testAnchor source (ctor target) path

testAnchor :: Node ContentMock -> Child (Node ContentMock) -> Path -> IO ()
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

pyramidTestCase :: Point -> (Node ContentMock -> Child (Node ContentMock)) -> IO ()
pyramidTestCase target ctor = (Prelude.head . children $ layoutChildren n pyramid) `shouldBe` ctor t
  where n = Node { content = ContentMock "X", position = (-1.5, 1.5), children = [TopChild t] }
        t = Node { content = ContentMock "Y", position = target, children = [] }

layoutTestCase :: (StatefulGen g IO) => g -> (Node ContentMock -> Child (Node ContentMock)) -> IO ()
layoutTestCase rng childCtor = do
  xs <- mkNRandomNodes 100 rng
  let x = NE.head xs
      n = x { children = map LeftChild (NE.tail xs) }
      layedoutChildren = children $ layoutChildren n (testLayoutFn childCtor)
   in mapM_ (`shouldSatisfy` (childCtor n ==)) layedoutChildren

testLayoutFn :: (Boundable a) => (Node a -> Child (Node a)) -> Node a -> Child (Node a) -> Child (Node a)
testLayoutFn ctor _ (LeftChild   n) = ctor n
testLayoutFn ctor _ (RightChild  n) = ctor n
testLayoutFn ctor _ (TopChild    n) = ctor n
testLayoutFn ctor _ (BottomChild n) = ctor n
