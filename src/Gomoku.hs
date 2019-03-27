{-# LANGUAGE FlexibleContexts #-}

module Gomoku
  ( initState
  , Cell(..)
  , AppState(..)
  , moveCursor
  , CursorDir (..)
  ) where

import Constant

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

-- Types
data AppState = AppState
  { goGrid :: [[Cell]] -- ^ go grid with piece
  , cursor :: Coord -- ^ user cursor for play
  }

type Coord = V2 Int

data Cell
  = PieceBlack
  | PieceWhite
  | Empty

-- makeLenses ''AppState
-- | Initialize the app state
initState :: AppState
initState =
  AppState {goGrid = [[Gomoku.PieceBlack | j <- [1 .. widthGoGrid]] | i <- [1 .. heightGoGrid]], cursor = V2 0 0}

data CursorDir
  = Up
  | Down
  | Right
  | Left

moveCursor :: AppState -> CursorDir -> Coord
moveCursor AppState {cursor = (V2 x y)} Gomoku.Up = V2 x (y - 1)
moveCursor AppState {cursor = (V2 x y)} Gomoku.Down = V2 x (y + 1)
moveCursor AppState {cursor = (V2 x y)} Gomoku.Right = V2 (x + 1) y
moveCursor AppState {cursor = (V2 x y)} Gomoku.Left = V2 (x - 1) y