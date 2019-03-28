module Reducer where

import Constant (hGoGrid)
import Control.Lens.Combinators (imap)
import System.Random (Random(..), newStdGen)

-- TYPES STATE
data AppState
  = GameState { goGrid :: [[Cell]] -- go grid with piece
              , gameMode :: GameMode -- solo or tow player
              , playerTurn :: Player -- turn of player
              , cursor :: Coord -- user cursor for play
              , cursorVisible :: Bool -- cursor alternator
               }
  | Home GameMode
  | SoloSelectPlayer Player

type Coord = (Int, Int)

data Cell
  = PieceBlack
  | PieceWhite
  | EmptyCell

data Player
  = PlayerWhite
  | PlayerBlack

data GameMode
  = GameSolo Player
  | GameMulti

-- INIT STATE
initState :: AppState
initState = Home (GameSolo PlayerWhite)

-- UPDATE STATE
data CursorDir
  = CursorUp
  | CursorDown
  | CursorRight
  | CursorLeft

moveCursor :: AppState -> CursorDir -> AppState
moveCursor s d =
  case s of
    GameState {cursor = (x, y)} -> s {cursor = upCoord d x y}
    _ -> s
  where
    upCoord d x y =
      case d of
        CursorUp -> (x, (y - 1) `mod` hGoGrid)
        CursorDown -> (x, (y + 1) `mod` hGoGrid)
        CursorRight -> ((x + 1) `mod` hGoGrid, y)
        CursorLeft -> ((x - 1) `mod` hGoGrid, y)

placePiece :: AppState -> AppState
placePiece s =
  case s of
    GameState {cursor = cr} -> s {goGrid = imap (upRow cr) (goGrid s), playerTurn = nextPlayer (playerTurn s)}
    _ -> s
  where
    upRow :: (Int, Int) -> Int -> [Cell] -> [Cell]
    upRow cr y = imap (upCell cr y)
    upCell (cx, cy) y x EmptyCell =
      if cx == x && cy == y
        then playerToPiece $ playerTurn s
        else EmptyCell
    upCell _ _ _ c = c

-- UTIL
playerToPiece :: Player -> Cell
playerToPiece PlayerWhite = PieceWhite
playerToPiece PlayerBlack = PieceBlack

nextPlayer :: Player -> Player
nextPlayer PlayerWhite = PlayerBlack
nextPlayer PlayerBlack = PlayerWhite