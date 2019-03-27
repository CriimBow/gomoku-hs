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

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

-- TYPES
data AppState = AppState
  { goGrid :: [[Cell]] -- ^ go grid with piece
  , cursor :: Coord -- ^ user cursor for play
  , cursorVisible :: Bool -- ^ cursor alternator
  , playerTurn :: Player
  }

type Coord = V2 Int

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

-- makeLenses ''AppState
-- INIT STATE
initState :: AppState
initState =
  AppState
    { goGrid = [[Gomoku.EmptyCell | j <- [1 .. hGoGrid]] | i <- [1 .. hGoGrid]]
    , cursor = V2 0 0
    , cursorVisible = False
    , playerTurn = PlayerWhite
    }

-- UPDATE STATE
moveCursor :: AppState -> CursorDir -> Coord
moveCursor AppState {cursor = (V2 x y)} Gomoku.Up = V2 x ((y - 1) `mod` hGoGrid)
moveCursor AppState {cursor = (V2 x y)} Gomoku.Down = V2 x ((y + 1) `mod` hGoGrid)
moveCursor AppState {cursor = (V2 x y)} Gomoku.Right = V2 ((x + 1) `mod` hGoGrid) y
moveCursor AppState {cursor = (V2 x y)} Gomoku.Left = V2 ((x - 1) `mod` hGoGrid) y

placePiece :: AppState -> AppState
placePiece s =
  let playerToPiece PlayerWhite = PieceWhite
      playerToPiece PlayerBlack = PieceBlack
      nextPlayer PlayerWhite = PlayerBlack
      nextPlayer PlayerBlack = PlayerWhite
      (V2 cx cy) = cursor s
      upRow y = imap (upCell y)
      upCell y x EmptyCell =
        if cx == x && cy == y
          then playerToPiece $ playerTurn s
          else EmptyCell
      upCell _ _ c = c
   in s {goGrid = imap upRow (goGrid s), playerTurn = nextPlayer (playerTurn s)}