module Main where

import           Control.Monad.RWS
import           Graphics.Vty
import           Potential.Core

data View = View { viewPortPos :: Point
                 , nodes       :: [Node Image]
                 }

instance Boundable Image where
  boundsOf im = Bounds (fromIntegral . imageWidth $ im) (fromIntegral. imageHeight $ im)

type Potential = RWST Vty () View IO

data Direction = North | East | South | West

main :: IO ()
main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  (w,h) <- displayBounds . outputIface $ vty
  initView <- mkInitView (fromIntegral w) (fromIntegral h)
  (finalView, ()) <- execRWST runPotential vty initView
  shutdown vty

mkInitView :: Float -> Float -> IO View
mkInitView w h = return View { viewPortPos = (-w/2, h/2)
                             , nodes = []
                             }

runPotential :: Potential ()
runPotential = do
  updateDisplay
  done <- processEvent
  unless done runPotential

updateDisplay :: Potential ()
updateDisplay = undefined

processEvent :: Potential Bool
processEvent = do
  k <- ask >>= liftIO . nextEvent
  if k == EvKey KEsc []
     then return True
     else do
       case k of
         EvKey (KChar 'h') [] -> moveCursor West
         EvKey (KChar 'j') [] -> moveCursor South
         EvKey (KChar 'k') [] -> moveCursor North
         EvKey (KChar 'l') [] -> moveCursor East
         _                    -> return ()
       return False

moveCursor :: Direction -> Potential ()
moveCursor North = undefined
moveCursor East  = undefined
moveCursor South = undefined
moveCursor West  = undefined
