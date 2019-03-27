{-# LANGUAGE FlexibleContexts #-}

module Gomoku
  ( initState
  , Cell(..)
  , AppState(..)
  , moveCursor
  , CursorDir(..)
  , placePiece
  ) where

import Constant (hGoGrid)

import Control.Lens.Combinators (imap)
import System.Random (Random(..), newStdGen)

-- TYPES
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

data CursorDir
  = Up
  | Down
  | Right
  | Left

-- INIT STATE
initState :: AppState
initState =
  AppState
    { goGrid = [[Gomoku.EmptyCell | j <- [1 .. hGoGrid]] | i <- [1 .. hGoGrid]]
    , cursor = (0, 0)
    , cursorVisible = False
    , playerTurn = PlayerWhite
    }

-- UPDATE STATE
moveCursor :: AppState -> CursorDir -> Coord
moveCursor AppState {cursor = (x, y)} Gomoku.Up = (x, ((y - 1) `mod` hGoGrid))
moveCursor AppState {cursor = (x, y)} Gomoku.Down = (x, ((y + 1) `mod` hGoGrid))
moveCursor AppState {cursor = (x, y)} Gomoku.Right = (((x + 1) `mod` hGoGrid), y)
moveCursor AppState {cursor = (x, y)} Gomoku.Left = (((x - 1) `mod` hGoGrid), y)

placePiece :: AppState -> AppState
placePiece s =
  let playerToPiece PlayerWhite = PieceWhite
      playerToPiece PlayerBlack = PieceBlack
      nextPlayer PlayerWhite = PlayerBlack
      nextPlayer PlayerBlack = PlayerWhite
      (cx, cy) = cursor s
      upRow y = imap (upCell y)
      upCell y x EmptyCell =
        if cx == x && cy == y
          then playerToPiece $ playerTurn s
          else EmptyCell
      upCell _ _ c = c
   in s {goGrid = imap upRow (goGrid s), playerTurn = nextPlayer (playerTurn s)}