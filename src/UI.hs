{-# LANGUAGE OverloadedStrings #-}

module UI where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

import Constant
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
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))

-- Custom event types
data Tick =
  Tick

-- Not currently used, but will be easier to refactor
type Name = ()

-- App definition
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
      threadDelay 100000 -- cursor switch
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initState

-- Handling events
handleEvent :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvent g (AppEvent Tick) = continue g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g

-- Drawing
drawUI :: AppState -> [Widget Name]
drawUI g = [C.center $ drawGrid g]

drawGrid :: AppState -> Widget Name
drawGrid AppState {goGrid = grd} = withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Gomoku") $ vBox rows
  where
    rows = map (hBox . cellsInRow) grd
    cellsInRow = map drawCell

drawCell :: Cell -> Widget Name
drawCell PieceWhite = withAttr pieceWhiteAttr cw
drawCell PieceBlack = withAttr pieceBlackAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

-- AttrMap
theMap :: AttrMap
theMap = attrMap V.defAttr [(pieceWhiteAttr, V.white `on` V.white), (pieceBlackAttr, V.green `on` V.green)]

pieceBlackAttr :: AttrName
pieceBlackAttr = "gameOver"

pieceWhiteAttr :: AttrName
pieceWhiteAttr = "snakeAttr"

emptyAttr :: AttrName
emptyAttr = "emptyAttr"