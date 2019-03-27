module Event
  ( handleEvent
  , Tick(..)
  ) where

import Brick (BrickEvent(..), EventM, Next, continue, halt)
import qualified Graphics.Vty as V
import Name (Name)
import qualified Reducer as R

-- EVENTS
data Tick =
  Tick

handleEvent :: R.AppState -> BrickEvent Name Tick -> EventM Name (Next R.AppState)
handleEvent g (AppEvent Tick) = continue $ g {R.cursorVisible = not (R.cursorVisible g)}
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ R.moveCursor g R.CursorUp
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ R.moveCursor g R.CursorDown
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ R.moveCursor g R.CursorRight
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ R.moveCursor g R.CursorLeft
handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ R.placePiece g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g