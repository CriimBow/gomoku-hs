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
    GameState {cursor = (x, y)} ->
      let coord =
            case d of
              CursorUp -> (x, (y - 1) `mod` hGoGrid)
              CursorDown -> (x, (y + 1) `mod` hGoGrid)
              CursorRight -> ((x + 1) `mod` hGoGrid, y)
              CursorLeft -> ((x - 1) `mod` hGoGrid, y)
       in s {cursor = coord}
    _ -> s

placePiece :: AppState -> AppState
placePiece s =
  let playerToPiece PlayerWhite = PieceWhite
      playerToPiece PlayerBlack = PieceBlack
      nextPlayer PlayerWhite = PlayerBlack
      nextPlayer PlayerBlack = PlayerWhite
      (cx, cy) = cursor s
      upRow :: Int -> [Cell] -> [Cell]
      upRow y = imap (upCell y)
      upCell y x EmptyCell =
        if cx == x && cy == y
          then playerToPiece $ playerTurn s
          else EmptyCell
      upCell _ _ c = c
   in s {goGrid = imap upRow (goGrid s), playerTurn = nextPlayer (playerTurn s)}