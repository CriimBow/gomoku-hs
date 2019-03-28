{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
  ( AttrMap
  , AttrName
  , Widget
  , attrMap
  , bg
  , hBox
  , on
  , padAll
  , padLeft
  , padLeftRight
  , padRight
  , str
  , vBox
  , withAttr
  , withBorderStyle
  )
import Control.Lens.Combinators (imap)
import Name (Name)

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import qualified Reducer as R
import Text.Printf (printf)

-- DRAWING
drawUI :: R.AppState -> [Widget Name]
drawUI g =
  case g of
    R.GameState {} -> appBox $ drawGame g
    R.Home mode -> appBox $ drawHome mode
    R.SoloSelectPlayer p -> appBox $ drawSoloSelectPlayer p
  where
    appBox w = [C.center $ withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Gomoku") w]

drawGame :: R.AppState -> Widget Name
drawGame R.GameState {R.goGrid = grd, R.cursor = (cx, cy), R.cursorVisible = crv} =
  hBox [padLeftRight 2 wInfo, wGoBoard, padLeftRight 2 wCmd]
  where
    wCmd :: Widget Name
    wCmd = str "Cmd"
    wInfo :: Widget Name
    wInfo = str "info"
    wGoBoard :: Widget Name
    wGoBoard = vBox $ [hBox $ map str boarderY] ++ imap cellsInRow grd ++ [hBox $ map str boarderY]
    boarderY = ["   "] ++ map padIntStr [0 .. 18] ++ [" "]
    padIntStr :: Int -> String
    padIntStr = printf " %.2d"
    cellsInRow y r = hBox $ [str $ printf "%.2d " y] ++ imap (drawCell y) r ++ [str $ padIntStr y]
    drawCell :: Int -> Int -> R.Cell -> Widget Name
    drawCell y x cell =
      if crv && cx == x && cy == y
        then case cell of
               R.EmptyCell -> withAttr cursorGoodAttr $ str "(#)" -- "() "
               _ -> withAttr cursorBadAttr $ str "(#)" -- "() "
        else case cell of
               R.PieceWhite -> withAttr pieceWhiteAttr $ str "(#)" -- "⚪  "
               R.PieceBlack -> withAttr pieceBlackAttr $ str "(#)" -- "⚫  "
               R.EmptyCell -> withAttr emptyAttr cw
    cw :: Widget Name
    cw = str "   "

drawHome :: R.GameMode -> Widget Name
drawHome mode = hBox wg
  where
    wg =
      case mode of
        R.GameSolo p -> [padAll 1 $ withAttr selected $ str "Solo", padAll 1 $ str "Multi"]
        R.GameMulti -> [padAll 1 $ str "Solo", padAll 1 $ withAttr selected $ str "Multi"]

drawSoloSelectPlayer :: R.Player -> Widget Name
drawSoloSelectPlayer p = vBox [padAll 1 $ str "What do you want to play ?", hBox wg]
  where
    wg =
      case p of
        R.PlayerWhite -> [padAll 1 $ withAttr selected $ str "White (1st)", str "  ", padAll 1 $ str "Black (2nd)"]
        R.PlayerBlack -> [padAll 1 $ str "White (1st)", str "  ", padAll 1 $ withAttr selected $ str "Black (2nd)"]

-- ATTR MAP
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (pieceBlackAttr, V.black `on` V.yellow `V.withStyle` V.bold)
    , (pieceWhiteAttr, V.white `on` V.yellow `V.withStyle` V.bold)
    , (cursorGoodAttr, V.green `on` V.yellow)
    , (cursorBadAttr, V.red `on` V.yellow)
    , (emptyAttr, V.yellow `on` V.yellow)
    , (selected, V.black `on` V.white)
    ]

pieceBlackAttr :: AttrName
pieceBlackAttr = "gameOver"

pieceWhiteAttr :: AttrName
pieceWhiteAttr = "snakeAttr"

cursorGoodAttr :: AttrName
cursorGoodAttr = "cursorGoodAttr"

cursorBadAttr :: AttrName
cursorBadAttr = "cursorBadAttr"

emptyAttr :: AttrName
emptyAttr = "emptyAttr"

selected :: AttrName
selected = "selected"
