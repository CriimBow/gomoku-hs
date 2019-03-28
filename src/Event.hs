module Event where

import Brick (BrickEvent(..), EventM, Next, continue, halt)
import Name (Name)

import qualified Graphics.Vty as V
import qualified Reducer as R

-- EVENTS
data CustomEvent =
  Tick

handleEvent :: R.AppState -> BrickEvent Name CustomEvent -> EventM Name (Next R.AppState)
handleEvent g (AppEvent Tick) =
  continue $
  case g of
    R.GameState {} -> g {R.cursorVisible = not (R.cursorVisible g)}
    _ -> g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) =
  continue $
  case g of
    R.GameState {} -> g {R.cursor = R.moveCursor g R.CursorUp}
    _ -> g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) =
  continue $
  case g of
    R.GameState {} -> g {R.cursor = R.moveCursor g R.CursorDown}
    _ -> g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) =
  continue $
  case g of
    R.GameState {} -> g {R.cursor = R.moveCursor g R.CursorRight}
    R.Home _ -> R.Home R.GameMulti
    R.SoloSelectPlayer _ -> R.SoloSelectPlayer R.PlayerBlack
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) =
  continue $
  case g of
    R.GameState {} -> g {R.cursor = R.moveCursor g R.CursorLeft}
    R.Home _ -> R.Home (R.GameSolo R.PlayerWhite)
    R.SoloSelectPlayer _ -> R.SoloSelectPlayer R.PlayerWhite
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) =
  continue $
  case g of
    R.GameState {} -> R.playerPlay g
    R.Home mode ->
      case mode of
        R.GameSolo _ -> R.SoloSelectPlayer R.PlayerWhite
        R.GameMulti -> R.initGameState R.GameMulti
    R.SoloSelectPlayer p -> R.initGameState (R.GameSolo p)
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) =
  continue $
  case g of
    R.GameState {} -> R.suggestionPlay g
    _ -> g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g
