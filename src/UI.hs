{-# LANGUAGE OverloadedStrings #-}

module UI where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

import Gomoku

import Brick
  ( App(..)
  , AttrMap
  , AttrName
  , BrickEvent(..)
  , EventM
  , Next
  , Padding(..)
  , Widget
  , (<+>)
  , attrMap
  , continue
  , customMain
  , emptyWidget
  , fg
  , hBox
  , hLimit
  , halt
  , neverShowCursor
  , on
  , padAll
  , padLeft
  , padRight
  , padTop
  , str
  , vBox
  , vLimit
  , withAttr
  , withBorderStyle
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import Control.Lens.Combinators (imap)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))

-- EVENTS
data Tick =
  Tick

handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent g (AppEvent Tick) = continue $ g {cursorVisible = not (cursorVisible g)}
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ g {cursor = moveCursor g Gomoku.Up}
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ g {cursor = moveCursor g Gomoku.Down}
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ g {cursor = moveCursor g Gomoku.Right}
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ g {cursor = moveCursor g Gomoku.Left}
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ placePiece g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g

-- DRAWING
drawUI :: AppState -> [Widget Name]
drawUI g = [C.center $ drawGrid g]

drawGrid :: AppState -> Widget Name
drawGrid AppState {goGrid = grd, cursor = cr, cursorVisible = crv} =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Gomoku") $ vBox rows
  where
    rows = imap cellsInRow grd
    cellsInRow y r = hBox $ imap (drawCell cr crv y) r

drawCell :: V2 Int -> Bool -> Int -> Int -> Cell -> Widget Name
drawCell (V2 cx cy) crv y x cell =
  if crv && cx == x && cy == y
    then withAttr cursorAttr cw
    else case cell of
           PieceWhite -> withAttr pieceWhiteAttr cw
           PieceBlack -> withAttr pieceBlackAttr cw
           EmptyCell -> withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

-- ATTR MAP
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (pieceBlackAttr, V.black `on` V.black)
    , (pieceWhiteAttr, V.white `on` V.white)
    , (cursorAttr, V.green `on` V.green)
    , (emptyAttr, V.blue `on` V.blue)
    ]

pieceBlackAttr :: AttrName
pieceBlackAttr = "gameOver"

pieceWhiteAttr :: AttrName
pieceWhiteAttr = "snakeAttr"

cursorAttr :: AttrName
cursorAttr = "cursorAttr"

emptyAttr :: AttrName
emptyAttr = "emptyAttr"

-- APP MAIN
-- Not currently used, but will be easier to refactor
type Name = ()

app :: App AppState Tick Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 300000 -- cursor alternator speed
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initState