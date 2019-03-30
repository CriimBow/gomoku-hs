module Reducer where

import Constant (hGoGrid)
import Control.Lens.Combinators (imap)
import System.CPUTime
import System.Random (Random(..), newStdGen)

-- TYPES STATE
data AppState
  = GameState { goGrid :: [[Cell]] -- go grid with piece
              , gameMode :: GameMode -- solo or tow player
              , playerTurn :: Player -- turn of player
              , lastIATimeForPlay :: Double -- time for IA play
              , cursorSuggestion :: Maybe Coord -- suggestion IA
              , cursor :: Coord -- user cursor for play
              , cursorVisible :: Bool -- cursor visibility alternator
              , end :: Maybe (Maybe Player) -- Maybe contain player win or Nothing if match null
              , nbPieceCapPWhite :: Int -- nombre de piece capture by player white
              , nbPieceCapPBlack :: Int -- nombre de piece capture by player black
               }
  | Home GameMode
  | SoloSelectPlayer Player

type Coord = (Int, Int)

data Cell
  = PieceBlack
  | PieceWhite
  | EmptyCell
  deriving (Eq)

data Player
  = PlayerWhite
  | PlayerBlack
  deriving (Eq)

data GameMode
  = GameSolo Player
  | GameMulti

-- INIT STATE
initState :: AppState
initState = Home (GameSolo PlayerWhite)

initGameState :: GameMode -> AppState
initGameState mode =
  GameState
    { goGrid =
        case mode of
          GameSolo PlayerBlack ->
            [ [ if i == 9 && j == 9
              then PieceWhite
              else EmptyCell
            | i <- [1 .. hGoGrid]
            ]
            | j <- [1 .. hGoGrid]
            ]
          _ -> [[EmptyCell | i <- [1 .. hGoGrid]] | j <- [1 .. hGoGrid]]
    , gameMode = mode
    , playerTurn =
        case mode of
          GameSolo PlayerBlack -> PlayerBlack
          _ -> PlayerWhite
    , lastIATimeForPlay = 0.0
    , cursorSuggestion = Nothing
    , cursor = (9, 9)
    , cursorVisible = True
    , end = Nothing
    , nbPieceCapPBlack = 0
    , nbPieceCapPWhite = 0
    }

-- UPDATE STATE
data CursorDir
  = CursorUp
  | CursorDown
  | CursorRight
  | CursorLeft

moveCursor :: AppState -> CursorDir -> (Int, Int)
moveCursor GameState {cursor = (x, y)} d =
  case d of
    CursorUp -> (x, (y - 1) `mod` hGoGrid)
    CursorDown -> (x, (y + 1) `mod` hGoGrid)
    CursorRight -> ((x + 1) `mod` hGoGrid, y)
    CursorLeft -> ((x - 1) `mod` hGoGrid, y)

handelPlayCoord :: Coord -> AppState -> AppState
handelPlayCoord cr s =
  case end s of
    Nothing ->
      if valideCoord cr (goGrid s)
        then let nwS = checkEnd $ checkCaptur cr $ s {goGrid = posePiece cr (playerTurn s) (goGrid s)}
              in nwS {playerTurn = nextPlayer (playerTurn s)}
        else s
    _ -> s

posePiece :: Coord -> Player -> [[Cell]] -> [[Cell]]
posePiece (cx, cy) p = imap upRow
  where
    upRow :: Int -> [Cell] -> [Cell]
    upRow y = imap (upCell y)
    upCell y x c =
      if cx == x && cy == y
        then playerToPiece p
        else c

checkCaptur :: Coord -> AppState -> AppState
checkCaptur cr s =
  let toSup = checkCapturToSup (playerTurn s) cr (goGrid s)
      nbCap = length toSup * 3
      newGrd = supPosGrid (goGrid s) toSup
   in case playerTurn s of
        PlayerBlack -> s {goGrid = newGrd, nbPieceCapPBlack = nbPieceCapPBlack s + nbCap}
        PlayerWhite -> s {goGrid = newGrd, nbPieceCapPWhite = nbPieceCapPWhite s + nbCap}

checkCapturToSup :: Player -> Coord -> [[Cell]] -> [[(Int, Int, Player)]]
checkCapturToSup p cr grd = filter (checkPoss grd) $ map (genPosCheck cr p) toCheck
  where
    toCheck = [(0, 1), (1, 0), (1, 1), (-1, -1), (-1, 0), (0, -1), (-1, 1), (1, -1)]
    genPosCheck (cx, cy) p (dx, dy) =
      [(cx + dx, cy + dy, nextPlayer p), (cx + dx * 2, cy + dy * 2, nextPlayer p), (cx + dx * 3, cy + dy * 3, p)]
    checkPoss grd psCks = length (filter (checkPos grd) psCks) == 3
    checkPos grd (x, y, p) = x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && grd !! y !! x == playerToPiece p

supPosGrid :: [[Cell]] -> [[(Int, Int, Player)]] -> [[Cell]]
supPosGrid = foldr supElGrd
  where
    supElGrd poss grd =
      let (fx, fy, _) = head poss
          (sx, sy, _) = poss !! 1
       in [ [ if (x == fx && y == fy) || (x == sx && y == sy)
            then EmptyCell
            else grd !! y !! x
          | x <- [0 .. hGoGrid - 1]
          ]
          | y <- [0 .. hGoGrid - 1]
          ]

handelIAPlay :: AppState -> IO AppState
handelIAPlay s = do
  start <- getCPUTime
  let mCoord = solver (goGrid s) (playerTurn s)
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 9)
  let withDiff = s {lastIATimeForPlay = diff}
  case mCoord of
    Nothing -> return withDiff
    Just crd -> return (handelPlayCoord crd withDiff)

suggestionPlay :: AppState -> IO AppState
suggestionPlay s = do
  let GameState {goGrid = grd, playerTurn = plTrn} = s
  start <- getCPUTime
  let coord = solver grd plTrn
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 9)
  return s {lastIATimeForPlay = diff, cursorSuggestion = coord}

-- UTIL
playerToPiece :: Player -> Cell
playerToPiece PlayerWhite = PieceWhite
playerToPiece PlayerBlack = PieceBlack

nextPlayer :: Player -> Player
nextPlayer PlayerWhite = PlayerBlack
nextPlayer PlayerBlack = PlayerWhite

valideCoords :: [[Cell]] -> [[Bool]] -- TODO
valideCoords = map (map (== EmptyCell))

valideCoord :: Coord -> [[Cell]] -> Bool
valideCoord (cx, cy) grd = cx >= 0 && cx < hGoGrid && cy >= 0 && cy < hGoGrid && valideCoords grd !! cy !! cx

checkEnd :: AppState -> AppState -- TODO
checkEnd s
  | nbPieceCapPWhite s >= 10 = s {end = Just (Just PlayerWhite)}
  | nbPieceCapPBlack s >= 10 = s {end = Just (Just PlayerBlack)}
  | False = s -- TODO alignement
  | hGoGrid == length (filter (\r -> 0 == length (filter id r)) $ valideCoords $ goGrid s) = s {end = Just Nothing}
  | otherwise = s

-- SOLVER
solver :: [[Cell]] -> Player -> Maybe Coord -- TODO
solver grd p = Just (0, 0)
