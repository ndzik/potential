{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.RWS
import           Data.List
import qualified Data.List.NonEmpty as NE (NonEmpty (..), map)
import           Fmt
import           Graphics.Vty       hiding (Mode)
import           Potential.Core
import           Potential.Layout

data View = View { viewPortPos    :: ViewPort
                 , viewPortDim    :: (Int, Int)
                 , nodes          :: [Node Image]
                 , cursorPosition :: Point
                 , mode           :: Mode
                 , userInput      :: Maybe String
                 }

instance Boundable Image where
  boundsOf im = Bounds w h
    where w = fromIntegral . imageWidth $ im
          h = fromIntegral. imageHeight $ im

type Potential = RWST Vty () View IO

data Mode = Command | CreateNode

newtype ViewPort = ViewPort Point

data Direction = North | East | South | West

main :: IO ()
main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  (w,h) <- displayBounds . outputIface $ vty
  initView <- mkInitView w h
  (_, ()) <- execRWST runPotential vty initView { nodes = map (`layoutChildren` pyramid) $ nodes initView }
  shutdown vty

mkInitView :: Int -> Int -> IO View
mkInitView w h = return View { viewPortPos = ViewPort (fromIntegral $ -w `div` 2, fromIntegral $ h `div` 2)
                             , viewPortDim = (w, h)
                             , nodes = []
                             , cursorPosition = (0, 0)
                             , mode = Command
                             , userInput = Nothing
                             }

runPotential :: Potential ()
runPotential = do
  gets mode >>= updateDisplay
  done <- processEvent
  unless done runPotential

updateDisplay :: Mode -> Potential ()
updateDisplay Command = do
  vty <- ask
  (cursorX, cursorY) <- cursorPositionVP
  info <- debugInfo
  ns <- drawNodes
  v <- get
  let oi = outputIface vty
      pic = picForLayers (info : ns)
  liftIO $ update vty pic { picCursor = Cursor (round cursorX) (round cursorY) }
updateDisplay CreateNode = do
  vty <- ask
  (w, h) <- liftIO $ displayBounds (outputIface vty)
  uinput <- gets userInput
  let background = resize w h emptyImage
      info = string defAttr "<Esc> to submit." <-> string defAttr "Insert content for Node:"
      input = mkImageForInput uinput
      pic = picForLayers [box defAttr (w-2) (h-2), translate 1 1 $ info <-> input, background]
  liftIO $ update vty pic

drawNodes :: Potential [Image]
drawNodes = do
  vp <- gets viewPortPos
  ns <- gets nodes
  return $ join $ map (drawNode vp) ns

drawNode :: ViewPort -> Node Image -> [Image]
drawNode vp n = translate (round x) (round y) c : drawConnections vp n
  where c = content n
        (Bounds w h) = boundsOf c
        (x, y) = toViewPort' (position n) vp

drawConnections :: ViewPort -> Node Image -> [Image]
drawConnections vp n = join $ map (drawPath vp) paths
  where c = children n
        paths = map (connect n) c

drawPath :: ViewPort -> Path -> [Image]
drawPath vp ap@(p NE.:| path) = go (p:path) []
  where go :: [Point] -> [Image] -> [Image]
        go [p] imgs    = imgs
        go (s:ps) imgs = go ps (translate (floor x) (floor y) (drawLine defAttr s (head ps)):imgs)
          where tgt = choosePathLocation s (head ps)
                (x, y) = toViewPort' tgt vp

choosePathLocation :: Point -> Point -> Point
choosePathLocation src@(x1, y1) tgt@(x2, y2)
  | x1 == x2 = if y1 >= y2 then src else tgt
  | y1 == y2 = if x1 >= x2 then tgt else src

mkImageForInput :: Maybe String -> Image
mkImageForInput Nothing  = emptyImage
mkImageForInput (Just s) = box' defAttr (round w) (round h) c
  where c = mkContentFrom s
        Bounds w h = boundsOf c

box :: Attr -> Int -> Int -> Image
box attr w h = box' attr w h (resize w h emptyImage)

box' :: Attr -> Int -> Int -> Image -> Image
box' attr w h c = (tlCorner attr <-> drawVerLine attr h <-> blCorner attr)
              <|> (drawHorLine attr w <-> c <-> drawHorLine attr w)
              <|> (trCorner attr <-> drawVerLine attr h <-> brCorner attr)

tlCorner :: Attr -> Image
tlCorner attr = char attr '┏'

trCorner :: Attr -> Image
trCorner attr = char attr '┓'

blCorner :: Attr -> Image
blCorner attr = char attr '┗'

brCorner :: Attr -> Image
brCorner attr = char attr '┛'

-- drawLine draws a line between the given two points. The points are assumed
-- to describe vertical or horizontal lines.
drawLine :: Attr -> Point -> Point -> Image
drawLine attr p1@(x1', y1') p2@(x2', y2')
  | x1 == x2 = drawVerLine attr (abs $ y2-y1)
  | otherwise = drawHorLine attr (abs $ x2-x1)
  where x1 = round x1'
        x2 = round x2'
        y1 = round y1'
        y2 = round y2'

drawVerLine :: Attr -> Int -> Image
drawVerLine attr len = foldr (<->) emptyImage [char attr '┃' | _ <- [1..len]]

drawHorLine :: Attr -> Int -> Image
drawHorLine attr len = foldr (<|>) emptyImage [char attr '━' | _ <- [1..len]]

debugInfo :: Potential Image
debugInfo = do
  ViewPort (viewPortX, viewPortY) <- gets viewPortPos
  (globalCursorX, globalCursorY) <- gets cursorPosition
  (vpCursorX, vpCursorY) <- cursorPositionVP
  return $ (foldr1 (<->) . map (string defAttr) $ ["Move viewport: Shift + <h,j,k,l>"
                                                  ,"Move cursor: <h,j,k,l>"
                                                  ,"Create Node: <Enter>"
                                                  ,"Quit: <q>"
                                                  ])
          <|> string defAttr " "
          <|> (foldr1 (<->) . map (string defAttr) $ ["ViewPort: ("+|viewPortX|+", "+|viewPortY|+")"
                                                     ,"Cursor G: ("+|globalCursorX|+", "+|globalCursorY|+")"
                                                     ,"Cursor L: ("+|vpCursorX|+", "+|vpCursorY|+")"])

cursorPositionVP :: Potential Point
cursorPositionVP = gets cursorPosition >>= toViewPort

processEvent :: Potential Bool
processEvent = do
  m <- gets mode
  k <- ask >>= liftIO . nextEvent
  case m of
    Command    -> processCommand k
    CreateNode -> processCreateNode k

processCreateNode :: Event -> Potential Bool
processCreateNode (EvKey KEsc []) = gets userInput >>= createNode >> updateMode Command >> continue
processCreateNode (EvKey (KChar c) []) = gets userInput >>= (`append` c) >> continue
processCreateNode (EvKey KBS m) = gets userInput >>= backspace m >> continue
processCreateNode (EvKey KEnter []) = gets userInput >>= (`append` '\n') >> continue
processCreateNode _ = continue

backspace :: [Modifier] -> Maybe String -> Potential ()
backspace _ Nothing      = return ()
backspace [] (Just input) = get >>= \v -> put v { userInput = case init input of
                                                                [] -> Nothing
                                                                s  -> Just s
                                                }

append :: Maybe String -> Char -> Potential ()
append Nothing c = do
  v <- get
  put v { userInput = Just [c] }
append (Just input) c = do
  v <- get
  put v { userInput = Just (input ++ [c]) }

continue :: Potential Bool
continue = return False

stop :: Potential Bool
stop = return True

processCommand :: Event -> Potential Bool
processCommand (EvKey (KChar 'q') [])        = stop
processCommand (EvKey (KChar 'h') [])        = moveCursor West >> continue
processCommand (EvKey (KChar 'j') [])        = moveCursor South >> continue
processCommand (EvKey (KChar 'k') [])        = moveCursor North >> continue
processCommand (EvKey (KChar 'l') [])        = moveCursor East >> continue
processCommand (EvKey (KChar 'H') [])        = moveViewPort West >> continue
processCommand (EvKey (KChar 'J') [])        = moveViewPort South >> continue
processCommand (EvKey (KChar 'K') [])        = moveViewPort North >> continue
processCommand (EvKey (KChar 'L') [])        = moveViewPort East >> continue
processCommand (EvKey KEnter [])             = updateMode CreateNode >> continue
processCommand (EvResize newWidth newHeight) = handleResize newWidth newHeight >> continue
processCommand _                             = continue

updateMode :: Mode -> Potential ()
updateMode m = get >>= \v -> put v { mode = m
                                   , userInput = Nothing
                                   }

handleResize :: Int -> Int -> Potential ()
handleResize newWidth newHeight = do
  oi <- asks outputIface
  v <- get
  let ViewPort (x, y) = viewPortPos v
      (w, h) = viewPortDim v
      dw = newWidth - w
      dh = newHeight - h
      newX = x - fromIntegral (dw `div` 2)
      newY = y + fromIntegral (dh `div` 2)
  put v { viewPortPos = ViewPort (newX, newY)
        , viewPortDim = (newWidth, newHeight)
        }

createNode :: Maybe String -> Potential ()
createNode Nothing = return ()
createNode (Just input) = do
  v <- get
  let c = mkContentFrom input
      Bounds w h = boundsOf c
      n = Node { content = box' defAttr (round w) (round h) c
               , position = cursorPosition v
               , children = []}
  put v { nodes = n : nodes v }

mkContentFrom :: String -> Image
mkContentFrom s = foldr1 vertJoin $ map (string defAttr) $ lines s


moveViewPort :: Direction -> Potential ()
moveViewPort North = moveViewPort' 0 1
moveViewPort East  = moveViewPort' 1 0
moveViewPort South = moveViewPort' 0 (-1)
moveViewPort West  = moveViewPort' (-1) 0

moveViewPort' :: Float -> Float -> Potential ()
moveViewPort' dx dy = do
  v <- get
  let ViewPort (x, y) = viewPortPos v
  put $ v { viewPortPos = ViewPort (x+dx, y+dy) }

moveCursor :: Direction -> Potential ()
moveCursor North = moveCursor' 0 1
moveCursor East  = moveCursor' 1 0
moveCursor South = moveCursor' 0 (-1)
moveCursor West  = moveCursor' (-1) 0

moveCursor' :: Float -> Float -> Potential ()
moveCursor' dx dy = do
  oi <- asks outputIface
  v <- get
  let (x, y) = cursorPosition v
  put $ v { cursorPosition = (x+dx, y+dy) }

-- toViewPort translates the given `Point` to ViewPort-coordinates.
toViewPort :: Point -> Potential Point
toViewPort globalCoord = do
  viewPortCoord <- gets viewPortPos
  return $ toViewPort' globalCoord viewPortCoord

-- toViewPort translates the given `Point` to the given viewport coordinates.
toViewPort' :: Point -> ViewPort -> Point
toViewPort' (globalX, globalY) (ViewPort (viewPortX, viewPortY)) = (globalX - viewPortX, viewPortY - globalY)
