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
    R.GameState {R.gameMode = gmdMd, R.playerTurn = plTrn} ->
      case R.posePiece (R.cursor g) (R.playerTurn g) (R.goGrid g) of
        Nothing -> continue g
        Just grd ->
          let nextG = g {R.goGrid = grd, R.playerTurn = R.nextPlayer (R.playerTurn g), R.cursorSuggestion = Nothing}
           in case gmdMd of
                R.GameMulti -> continue nextG
                R.GameSolo p -> liftIO (handelGameSoloPlay p nextG) >>= continue
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

-- UTIL
handelGameSoloPlay :: R.Player -> R.AppState -> IO R.AppState
handelGameSoloPlay p s = do
  start <- getCPUTime
  let mCoord = R.solver (R.goGrid s) (R.playerTurn s)
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 9)
  let withDiff = s {R.lastIATimeForPlay = diff, R.cursorSuggestion = Nothing}
  let nGS =
        case mCoord of
          Nothing -> withDiff
          Just cod ->
            case R.posePiece cod (R.playerTurn withDiff) (R.goGrid withDiff) of
              Nothing -> withDiff
              Just newGrd -> withDiff {R.goGrid = newGrd, R.playerTurn = R.nextPlayer (R.playerTurn withDiff)}
  return nGS