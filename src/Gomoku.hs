{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Gomoku
  ( initState
  , Cell(..)
  , AppState(..)
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
initState = AppState {goGrid = [[Gomoku.PieceBlack | j <- [1 .. widthGoGrid]] | i <- [1 .. heightGoGrid]], cursor = V2 0 0}