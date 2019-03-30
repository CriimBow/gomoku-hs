module Event where

import Brick (BrickEvent(..), EventM, Next, continue, halt)
import Control.Monad.IO.Class (liftIO)
import Name (Name)
import System.CPUTime

import qualified Graphics.Vty as V
import qualified Reducer as R

-- EVENTS
data CustomEvent =
  Tick

-- END GAME ?
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
  case g of
    R.GameState {R.gameMode = gmdMd, R.playerTurn = plTrn, R.cursor = cr, R.goGrid = grd} ->
      let nextG = R.handelPlayCoord cr (g {R.cursorSuggestion = Nothing})
       in case gmdMd of
            R.GameMulti -> continue nextG
            R.GameSolo _ -> liftIO (R.handelIAPlay nextG) >>= continue
    R.Home mode ->
      case mode of
        R.GameSolo _ -> continue $ R.SoloSelectPlayer R.PlayerWhite
        R.GameMulti -> continue $ R.initGameState R.GameMulti
    R.SoloSelectPlayer p -> continue $ R.initGameState (R.GameSolo p)
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) =
  case g of
    R.GameState {} -> liftIO (R.suggestionPlay g) >>= continue
    _ -> continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) =
  case g of
    R.Home _ -> halt g
    _ -> continue $ R.Home (R.GameSolo R.PlayerWhite)
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g
