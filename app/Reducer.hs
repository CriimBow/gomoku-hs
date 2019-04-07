module Reducer where

import Constant (allDir, hGoGrid)
import Control.Lens.Combinators (imap)
import Data.Maybe (isJust, isNothing)
import Debug.Trace (trace, traceIO, traceShow)
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
  deriving (Eq, Show)

data Cell
  = PieceBlack
  | PieceWhite
  | EmptyCell
  deriving (Eq, Show)

data Player
  = PlayerWhite
  | PlayerBlack
  deriving (Eq, Show)

data GameMode
  = GameSolo Player
  | GameMulti
  deriving (Eq, Show)

type Coord = (Int, Int)

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
      if validCoord (goGrid s) (playerTurn s) cr
        then let nwS = checkEnd cr $ checkCaptur cr $ s {goGrid = posePiece cr (playerTurn s) (goGrid s)}
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

posePieceAndDelete :: Coord -> Player -> [[Cell]] -> [[Cell]]
posePieceAndDelete cr p grd =
  let withPiece = posePiece cr p grd
      toSup = checkCapturToSup p cr withPiece
      newGrd = supPosGrid withPiece toSup
   in newGrd

checkCaptur :: Coord -> AppState -> AppState
checkCaptur cr s =
  let toSup = checkCapturToSup (playerTurn s) cr (goGrid s)
      nbCap = length toSup * 2
      newGrd = supPosGrid (goGrid s) toSup
   in case playerTurn s of
        PlayerBlack -> s {goGrid = newGrd, nbPieceCapPBlack = nbPieceCapPBlack s + nbCap}
        PlayerWhite -> s {goGrid = newGrd, nbPieceCapPWhite = nbPieceCapPWhite s + nbCap}

checkCapturToSup :: Player -> Coord -> [[Cell]] -> [[(Int, Int, Player)]]
checkCapturToSup p cr grd = filter (checkPoss grd) $ map (genPosCheck cr p) allDir
  where
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
suggestionPlay s =
  if isJust (end s)
    then return s
    else do
      start <- getCPUTime
      let coord = solver (goGrid s) (playerTurn s)
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

validCoords :: [[Cell]] -> Player -> [[Bool]]
validCoords grd p =
  let emptyCells = map (map (== EmptyCell)) grd
      msk = (maskCoef $ playerToPiece p)
   in [[emptyCells !! y !! x && checkDoubleThree grd msk (x, y) | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]
  where
    maskCoef pc =
      [ [(-3, EmptyCell), (-2, pc), (-1, pc), (0, EmptyCell), (1, EmptyCell)]
      , [(-2, EmptyCell), (-1, pc), (0, EmptyCell), (1, pc), (2, EmptyCell)]
      , [(-4, EmptyCell), (-3, pc), (-2, pc), (-1, EmptyCell), (0, EmptyCell), (1, EmptyCell)]
      , [(-2, EmptyCell), (-1, pc), (0, EmptyCell), (1, EmptyCell), (2, pc), (3, EmptyCell)]
      , [(-1, EmptyCell), (0, EmptyCell), (1, pc), (2, EmptyCell), (3, pc), (4, EmptyCell)]
      ]
    checkDoubleThree grd msk cr = checkAllPos grd $ allDir >>= genPosCheck msk cr
    genPosCheck :: [[(Int, Cell)]] -> Coord -> Coord -> [([(Int, Int, Cell)], (Int, Int))]
    genPosCheck msk (cx, cy) (dx, dy) = map (\r -> (map (\(k, c) -> (cx + dx * k, cy + dy * k, c)) r, (dx, dy))) msk
    checkAllPos :: [[Cell]] -> [([(Int, Int, Cell)], (Int, Int))] -> Bool
    checkAllPos grd lpos =
      let tmp = map snd $ filter (checkLPos grd) lpos
          dDir = foldr delDir [] tmp
       in 1 >= length dDir
    checkLPos :: [[Cell]] -> ([(Int, Int, Cell)], (Int, Int)) -> Bool
    checkLPos grd (lp, dir) = length lp == length (filter (checkPos grd) lp)
    checkPos :: [[Cell]] -> (Int, Int, Cell) -> Bool
    checkPos grd (x, y, pc) = x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && grd !! y !! x == pc
    delDir :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    delDir (drx, dry) acc = filter (\(dx, dy) -> not (drx == negate dx && dry == negate dy)) $ acc ++ [(drx, dry)]

validCoord :: [[Cell]] -> Player -> Coord -> Bool
validCoord grd p (cx, cy) = cx >= 0 && cx < hGoGrid && cy >= 0 && cy < hGoGrid && validCoords grd p !! cy !! cx

-- False if is dist <= maxDist
distEmptyCellMap :: [[Cell]] -> Int -> [[Bool]]
distEmptyCellMap grd maxDist =
  let initMap = map (map (== EmptyCell)) grd
      iterator = [1 .. maxDist]
   in foldr (\_ b -> addDist1 b) initMap iterator
  where
    addDist1 :: [[Bool]] -> [[Bool]]
    addDist1 grd = [[grd !! y !! x && not (checkNeighbour grd x y) | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]
    checkNeighbour :: [[Bool]] -> Int -> Int -> Bool
    checkNeighbour grd x y =
      checkPos grd (x + 1) y || checkPos grd x (y + 1) || checkPos grd x (y - 1) || checkPos grd (x - 1) y
    checkPos :: [[Bool]] -> Int -> Int -> Bool
    checkPos grd x y = x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && not (grd !! y !! x)

-- /!\ no valide play if the map is Empty!
validIACoords :: [[Cell]] -> Player -> Int -> [[Bool]]
validIACoords grd p d =
  let v = validCoords grd p
      gd = distEmptyCellMap grd d
   in [[v !! y !! x && not (gd !! y !! x) | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]

validCoordToList :: [[Bool]] -> [(Int, Int)]
validCoordToList grid =
  [(x,y) | x <- [0 .. hGoGrid - 1], y <- [0 .. hGoGrid - 1], grid !! y !! x]

checkEnd :: Coord -> AppState -> AppState
checkEnd cr s
  | nbPieceCapPWhite s >= 10 = s {end = Just (Just PlayerWhite)}
  | nbPieceCapPBlack s >= 10 = s {end = Just (Just PlayerBlack)}
  | checkAlign5 cr (goGrid s) (playerTurn s) = s {end = Just (Just (playerTurn s))}
  | hGoGrid == length (filter (\r -> 0 == length (filter id r)) $ validCoords (goGrid s) (playerTurn s)) =
    s {end = Just Nothing}
  | otherwise = s
  where
    checkAlign5 (cx, cy) grd p = checkAllPos grd p $ allDir >>= genPosCheck cr
    maskCoef = [[-4, -3, -2, -1, 0], [-3, -2, -1, 0, 1], [-2, -1, 0, 1, 2]]
    genPosCheck :: Coord -> Coord -> [[Coord]]
    genPosCheck (cx, cy) (dx, dy) = map (map (\k -> (cx + dx * k, cy + dy * k))) maskCoef
    checkAllPos grd p lpos =
      let tmp = map (length . filter (checkPos grd p)) lpos
       in (0 /= length (filter (== 5) tmp))
    checkPos grd p (x, y) = x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && grd !! y !! x == playerToPiece p

------------
-- SOLVER --
------------
solver :: [[Cell]] -> Player -> Maybe Coord -- TODO
solver grd p = Just (0, 0)
