module Event where

import Brick (BrickEvent(..), EventM, Next, continue, halt)
import Control.Monad.IO.Class (liftIO)
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
    R.GameState {R.gameMode = gmdMd, R.playerTurn = plTrn} ->
      case R.posePiece (R.cursor g) (R.playerTurn g) (R.goGrid g) of
        Nothing -> g
        Just grd ->
          case gmdMd of
            R.GameMulti ->
              g {R.goGrid = grd, R.playerTurn = R.nextPlayer (R.playerTurn g), R.cursorSuggestion = Nothing}
            R.GameSolo p ->
              let gTmp =
                    if p == plTrn
                      then g
                             { R.goGrid = grd
                             , R.playerTurn = R.nextPlayer (R.playerTurn g)
                             , R.cursorSuggestion = Nothing
                             }
                      else g
               in case R.solver (R.goGrid gTmp) (R.playerTurn gTmp) -- TODO time
                        of
                    Nothing -> gTmp
                    Just cod ->
                      case R.posePiece cod (R.playerTurn gTmp) (R.goGrid gTmp) of
                        Nothing -> gTmp
                        Just newGrd ->
                          gTmp
                            { R.goGrid = newGrd
                            , R.playerTurn = R.nextPlayer (R.playerTurn gTmp)
                            , R.cursorSuggestion = Nothing
                            }
    R.Home mode ->
      case mode of
        R.GameSolo _ -> R.SoloSelectPlayer R.PlayerWhite
        R.GameMulti -> R.initGameState R.GameMulti
    R.SoloSelectPlayer p -> R.initGameState (R.GameSolo p)
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