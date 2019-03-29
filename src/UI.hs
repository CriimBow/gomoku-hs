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
drawGame R.GameState { R.goGrid = grd
                     , R.cursor = (cx, cy)
                     , R.cursorVisible = crv
                     , R.playerTurn = plTurn
                     , R.lastIATimeForPlay = lstTimCmp
                     , R.cursorSuggestion = sugCrd
                     , R.end = end
                     } = hBox [padLeftRight 2 wInfo, padAll 2 wGoBoard, padLeftRight 2 wCmd]
    -- CMD
  where
    wCmd :: Widget Name
    wCmd = str "Cmd"
    -- INFO
    wInfo :: Widget Name
    wInfo = vBox ([str "time of last computation:", str $ printf "%f ms" lstTimCmp] ++ endMsg)
    endMsg =
      case end of
        Nothing ->
          case plTurn of
            R.PlayerBlack -> [str "Player Turn: Black"]
            R.PlayerWhite -> [str "Player Turn: White"]
        Just mPl ->
          case mPl of
            Nothing -> [str "match null"]
            Just plWin ->
              case plWin of
                R.PlayerWhite -> [str "Player White Win !"]
                R.PlayerBlack -> [str "Player Black Win !"]
    -- BOARD
    wGoBoard :: Widget Name
    wGoBoard = vBox $ [hBox $ map str boarderY] ++ imap cellsInRow grd ++ [hBox $ map str boarderY]
    boarderY = ["   "] ++ map (printf " %.2d") [0 .. 18 :: Int] ++ [" "]
    cellsInRow y r = hBox $ [str $ printf "%.2d " y] ++ imap (drawCell y) r ++ [str $ printf " %.2d" y]
    drawCell :: Int -> Int -> R.Cell -> Widget Name
    drawCell y x cell =
      if crv
        then if cx == x && cy == y
               then if not (R.valideCoord (cx, cy) grd)
                      then withAttr cursorBadAttr cw
                      else case R.playerToPiece plTurn of
                             R.PieceWhite -> withAttr pieceWhiteAttr cw
                             R.PieceBlack -> withAttr pieceBlackAttr cw
               else case sugCrd of
                      Just (csx, csy) ->
                        if csx == x && csy == y
                          then withAttr suggestionAttr cw
                          else pieceToWiget cell
                      Nothing -> pieceToWiget cell
        else pieceToWiget cell
    pieceToWiget p =
      case p of
        R.PieceWhite -> withAttr pieceWhiteAttr $ str "(#)" -- "⚪  "
        R.PieceBlack -> withAttr pieceBlackAttr $ str "(#)" -- "⚫  "
        R.EmptyCell -> withAttr emptyAttr cw
    cw :: Widget Name
    cw = str "{#}" -- "() "

drawHome :: R.GameMode -> Widget Name
drawHome mode = hBox wg
  where
    wg =
      case mode of
        R.GameSolo p -> [padAll 1 $ withAttr selectedAttr $ str "Solo", padAll 1 $ str "Multi"]
        R.GameMulti -> [padAll 1 $ str "Solo", padAll 1 $ withAttr selectedAttr $ str "Multi"]

drawSoloSelectPlayer :: R.Player -> Widget Name
drawSoloSelectPlayer p = vBox [padAll 1 $ str "What do you want to play ?", hBox wg]
  where
    wg =
      case p of
        R.PlayerWhite -> [padAll 1 $ withAttr selectedAttr $ str "White (1st)", str "  ", padAll 1 $ str "Black (2nd)"]
        R.PlayerBlack -> [padAll 1 $ str "White (1st)", str "  ", padAll 1 $ withAttr selectedAttr $ str "Black (2nd)"]

-- ATTR MAP
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (pieceBlackAttr, V.black `on` V.yellow `V.withStyle` V.bold)
    , (pieceWhiteAttr, V.white `on` V.yellow `V.withStyle` V.bold)
    , (cursorBadAttr, V.red `on` V.yellow `V.withStyle` V.bold)
    , (suggestionAttr, V.cyan `on` V.yellow `V.withStyle` V.bold)
    , (emptyAttr, V.yellow `on` V.yellow)
    , (selectedAttr, V.black `on` V.white)
    ]

pieceBlackAttr :: AttrName
pieceBlackAttr = "gameOver"

pieceWhiteAttr :: AttrName
pieceWhiteAttr = "snakeAttr"

cursorBadAttr :: AttrName
cursorBadAttr = "cursorBadAttr"

emptyAttr :: AttrName
emptyAttr = "emptyAttr"

selectedAttr :: AttrName
selectedAttr = "selected"

suggestionAttr :: AttrName
suggestionAttr = "suggestion"