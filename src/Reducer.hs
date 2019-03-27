module Reducer where

import Constant (hGoGrid)
import Control.Lens.Combinators (imap)
import System.Random (Random(..), newStdGen)

-- TYPES STATE
data AppState = AppState
  { goGrid :: [[Cell]] -- go grid with piece
  , cursor :: Coord -- user cursor for play
  , cursorVisible :: Bool -- cursor alternator
  , playerTurn :: Player -- turn of player
  }

type Coord = (Int, Int)

data Cell
  = PieceBlack
  | PieceWhite
  | EmptyCell

data Player
  = PlayerWhite
  | PlayerBlack

-- INIT STATE
initState :: AppState
initState =
  AppState
    { goGrid = [[EmptyCell | j <- [1 .. hGoGrid]] | i <- [1 .. hGoGrid]]
    , cursor = (0, 0)
    , cursorVisible = False
    , playerTurn = PlayerWhite
    }

-- UPDATE STATE
data CursorDir
  = CursorUp
  | CursorDown
  | CursorRight
  | CursorLeft

moveCursor :: AppState -> CursorDir -> AppState
moveCursor s d =
  let AppState {cursor = (x, y)} = s
      coord =
        case d of
          CursorUp -> (x, (y - 1) `mod` hGoGrid)
          CursorDown -> (x, (y + 1) `mod` hGoGrid)
          CursorRight -> ((x + 1) `mod` hGoGrid, y)
          CursorLeft -> ((x - 1) `mod` hGoGrid, y)
   in s {cursor = coord}

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
