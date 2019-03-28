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

initGameState :: GameMode -> AppState
initGameState mode =
  GameState
    { goGrid = [[EmptyCell | i <- [1 .. hGoGrid]] | j <- [1 .. hGoGrid]]
    , gameMode = mode
    , playerTurn = PlayerWhite
    , cursor = (9, 9)
    , cursorVisible = True
    }

-- UPDATE STATE
data CursorDir
  = CursorUp
  | CursorDown
  | CursorRight
  | CursorLeft

moveCursor :: AppState -> CursorDir -> (Int, Int)
moveCursor GameState {cursor = (x, y)} d =
  case d of
    CursorUp -> (x, (y - 1) `mod` hGoGrid)
    CursorDown -> (x, (y + 1) `mod` hGoGrid)
    CursorRight -> ((x + 1) `mod` hGoGrid, y)
    CursorLeft -> ((x - 1) `mod` hGoGrid, y)

playerPlay :: AppState -> AppState
playerPlay s =
  let GameState {cursor = (cx, cy), goGrid = grd} = s
      cellCr = grd !! cy !! cx
   in case cellCr of
        EmptyCell -> s {goGrid = imap (upRow (cx, cy)) (goGrid s), playerTurn = nextPlayer (playerTurn s)}
        _ -> s
  where
    upRow :: (Int, Int) -> Int -> [Cell] -> [Cell]
    upRow cr y = imap (upCell cr y)
    upCell (cx, cy) y x c =
      if cx == x && cy == y
        then playerToPiece $ playerTurn s
        else c

-- UTIL
playerToPiece :: Player -> Cell
playerToPiece PlayerWhite = PieceWhite
playerToPiece PlayerBlack = PieceBlack

nextPlayer :: Player -> Player
nextPlayer PlayerWhite = PlayerBlack
nextPlayer PlayerBlack = PlayerWhite