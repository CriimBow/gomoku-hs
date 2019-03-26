{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Gomoku
  ( initState
  , Cell(..)
  , AppState(..)
  ) where

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
  { _goGrid :: [[Cell]] -- ^ go grid with piece
  , _cursor :: Coord -- ^ user cursor for play
  }

type Coord = V2 Int

data Cell
  = PieceBlack
  | PieceWhite
  | Empty

makeLenses ''AppState

-- Constants
height :: Int
height = 19

width :: Int
width = 19

-- | Initialize the app state
initState :: AppState
initState = AppState {_goGrid = [[Gomoku.Empty | j <- [1 .. width]] | i <- [1 .. height]], _cursor = V2 0 0}