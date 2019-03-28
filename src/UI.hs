{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick (AttrMap, AttrName, Widget, attrMap, bg, hBox, on, str, vBox, withAttr, withBorderStyle)
import Control.Lens.Combinators (imap)
import Name (Name)

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import qualified Reducer as R

-- DRAWING
drawUI :: R.AppState -> [Widget Name]
drawUI g =
  case g of
    R.GameState {} -> appBox $ drawGame g
    R.Home mode -> appBox $ drawHome mode
    R.SoloSelectPlayer p -> appBox $ drawSoloSelectPlayer p
  where
    appBox w = [C.center $ withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Gomoku") $ vBox w]

drawGame :: R.AppState -> [Widget Name]
drawGame R.GameState {R.goGrid = grd, R.cursor = (cx, cy), R.cursorVisible = crv} = imap cellsInRow grd
  where
    cellsInRow y r = hBox $ imap (drawCell y) r
    drawCell :: Int -> Int -> R.Cell -> Widget Name
    drawCell y x cell =
      if crv && cx == x && cy == y
        then withAttr cursorAttr cw
        else case cell of
               R.PieceWhite -> withAttr pieceWhiteAttr cw
               R.PieceBlack -> withAttr pieceBlackAttr cw
               R.EmptyCell -> withAttr emptyAttr cw
      where
        cw :: Widget Name
        cw = str "  "

drawHome :: R.GameMode -> [Widget Name]
drawHome mode = [hBox wg]
  where
    wg =
      case mode of
        R.GameSolo p -> [withAttr selected $ str "Solo", str " ", str "Multi"]
        R.GameMulti -> [str "Solo", str " ", withAttr selected $ str "Multi"]

drawSoloSelectPlayer :: R.Player -> [Widget Name]
drawSoloSelectPlayer p = [hBox wg]
  where
    wg =
      case p of
        R.PlayerWhite -> [withAttr selected $ str "White", str " ", str "Black"]
        R.PlayerBlack -> [str "White", str " ", withAttr selected $ str "Black"]

-- ATTR MAP
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (pieceBlackAttr, V.black `on` V.black)
    , (pieceWhiteAttr, V.white `on` V.white)
    , (cursorAttr, V.green `on` V.green)
    , (emptyAttr, V.blue `on` V.blue)
    , (selected, V.black `on` V.white)
    ]

pieceBlackAttr :: AttrName
pieceBlackAttr = "gameOver"

pieceWhiteAttr :: AttrName
pieceWhiteAttr = "snakeAttr"

cursorAttr :: AttrName
cursorAttr = "cursorAttr"

emptyAttr :: AttrName
emptyAttr = "emptyAttr"

selected :: AttrName
selected = "selected"