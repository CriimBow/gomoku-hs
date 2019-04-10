module Reducer where

import Constant (allDir, hGoGrid)
import Control.DeepSeq
import Control.Lens.Combinators (imap)
import Data.List (foldl', sortBy)
import qualified Data.Vector.Unboxed as Vec
import Data.Char (chr)

-- import Control.Parallel (par)
-- import Data.List (elemIndex)
-- import Data.List.Split (chunksOf)
import Data.Maybe (isJust)

import System.CPUTime

-- import System.IO
-- import System.Random (Random(..), newStdGen)
-- TYPES STATE
data AppState
  = GameState { goGrid :: Grid -- go grid with piece
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

type Grid = Vec.Vector Char -- length 19 * 19

type GridBool = Vec.Vector Bool -- length 19 * 19

-- INIT STATE
initState :: AppState
initState = Home (GameSolo PlayerWhite)

initGameState :: GameMode -> AppState
initGameState mode =
  let grid = Vec.replicate (hGoGrid * hGoGrid) $ cellToChar EmptyCell
  in GameState
      { goGrid =
          case mode of
            GameSolo PlayerBlack ->
              Vec.imap putFirstPiece grid
            _ -> grid
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
  where
    putFirstPiece :: Int -> Char -> Char
    putFirstPiece idx _
        | idx == hGoGrid * 8 + 8 = cellToChar PieceWhite
        | otherwise = cellToChar EmptyCell

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

posePiece :: Coord -> Player -> Grid -> Grid
posePiece (cx, cy) p grid = Vec.imap putPiece grid
  where
    putPiece :: Int -> Char -> Char
    putPiece idx c = if idx == hGoGrid * cy + cx
                     then playerToPieceVec p
                     else c

posePieceAndDelete :: Coord -> Player -> Grid -> Grid
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

-- To modify

mapMemoCapturToSup :: ([[[[(Int, Int, Player)]]]], [[[[(Int, Int, Player)]]]])
mapMemoCapturToSup =
  let mw = [[map (genPosCheck (x, y) PlayerWhite) allDir | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]
      mb = [[map (genPosCheck (x, y) PlayerBlack) allDir | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]
   in (mw, mb)
  where
    genPosCheck (cx, cy) player (dx, dy) =
      [ (cx + dx, cy + dy, nextPlayer player)
      , (cx + dx * 2, cy + dy * 2, nextPlayer player)
      , (cx + dx * 3, cy + dy * 3, player)
      ]

checkCapturToSup :: Player -> Coord -> Grid -> [[(Int, Int, Player)]]
checkCapturToSup p (cx, cy) grd =
  let (mw, mn) = mapMemoCapturToSup
      toCheck =
        if p == PlayerWhite
          then mw
          else mn
   in Vec.filter (checkPoss grd) $ toCheck !! (hGoGrid * cy + cx)
  where
    checkPoss grid psCks = Vec.length (Vec.filter (checkPos grid) psCks) == 3
    checkPos gd (x, y, plr) = x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && gd !! (hGoGrid * y + x) == playerToPieceVec plr

supPosGrid :: GridBool -> [[(Int, Int, Player)]] -> GridBool
supPosGrid grd toSup = foldl' supElGrd grd toSup
  where
    supElGrd :: GridBool -> [(Int, Int, Player)] -> GridBool
    supElGrd grd poss =
      let (fx, fy, _) = head poss
          (sx, sy, _) = poss !! 1
       in Vec.imap (\i e -> if i == (fy * hGoGrid + fx) || i == (sy * hGoGrid + sx) then cellToChar EmptyCell else e) grd

handelIAPlay :: AppState -> IO AppState
handelIAPlay s = do
  start <- getCPUTime
  let mCoord = solver (goGrid s) (playerTurn s) (nbPieceCapPWhite s) (nbPieceCapPBlack s)
  endTimer <- mCoord `deepseq` getCPUTime
  let diff = fromIntegral (endTimer - start) / (10 ^ 9)
  let withDiff = s {lastIATimeForPlay = diff}
  return (handelPlayCoord mCoord withDiff)

suggestionPlay :: AppState -> IO AppState
suggestionPlay s =
  if isJust (end s)
    then return s
    else do
      start <- getCPUTime
      let coord = solver (goGrid s) (playerTurn s) (nbPieceCapPWhite s) (nbPieceCapPBlack s)
      endTimer <- coord `deepseq` getCPUTime
      let diff = fromIntegral (endTimer - start) / (10 ^ 9)
      return s {lastIATimeForPlay = diff, cursorSuggestion = Just coord}

-- UTIL
playerToPiece :: Player -> Cell
playerToPiece PlayerWhite = PieceWhite
playerToPiece PlayerBlack = PieceBlack

nextPlayer :: Player -> Player
nextPlayer PlayerWhite = PlayerBlack
nextPlayer PlayerBlack = PlayerWhite

playerToPieceVec :: Player -> Cell
playerToPieceVec PlayerWhite = '1'
playerToPieceVec PlayerBlack = '2'

cellToChar :: Cell -> Char
cellToChar EmptyCell = '0'
cellToChar PieceWhite = '1'
cellToChar PieceBlack = '2'

charToCell :: Char -> Cell
charToCell '0' = EmptyCell
charToCell '1' = PieceWhite
charToCell '2' = PieceBlack

-- can use delDoubleThree
validCoords :: Grid -> Player -> GridBool
validCoords grd p = delDoubleThree grd p (Vec.map (== cellToChar EmptyCell) grd)

validCoord :: Grid -> Player -> Coord -> Bool
validCoord grd p (cx, cy) = cx >= 0 && cx < hGoGrid && cy >= 0 && cy < hGoGrid && validCoords grd p !! cy * hGoGrid + cx

validCoordToList :: GridBool -> [(Int, Int)]
validCoordToList grid = [(x, y) | x <- [0 .. hGoGrid - 1], y <- [0 .. hGoGrid - 1], grid !! y * hGoGrid + x]

checkEnd :: Coord -> AppState -> AppState
checkEnd cr s
  | nbPieceCapPWhite s >= 10 = s {end = Just (Just PlayerWhite)}
  | nbPieceCapPBlack s >= 10 = s {end = Just (Just PlayerBlack)}
  | checkAlign5 cr (goGrid s) (playerTurn s) = s {end = Just (Just (playerTurn s))}
  | hGoGrid == length (filter (\r -> 0 == length (filter id r)) $ validCoords (goGrid s) (playerTurn s)) =
    s {end = Just Nothing}
  | otherwise = s
  where
    checkAlign5 _ grd p = checkAllPos grd p $ allDir >>= genPosCheck cr
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
mapMemoDoubleThree :: ([[[([(Int, Int, Cell)], (Int, Int))]]], [[[([(Int, Int, Cell)], (Int, Int))]]])
mapMemoDoubleThree =
  let maskWhite = maskCoef $ playerToPiece PlayerWhite
      maskBlack = maskCoef $ playerToPiece PlayerBlack
      genWhite = [[allDir >>= genPosCheck maskWhite (x, y) | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]
      genBlack = [[allDir >>= genPosCheck maskBlack (x, y) | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]
   in (genWhite, genBlack)
  where
    maskCoef :: Cell -> [[(Int, Cell)]]
    maskCoef pc =
      [ [(-3, EmptyCell), (-2, pc), (-1, pc), (0, EmptyCell), (1, EmptyCell)]
      , [(-2, EmptyCell), (-1, pc), (0, EmptyCell), (1, pc), (2, EmptyCell)]
      , [(-4, EmptyCell), (-3, pc), (-2, pc), (-1, EmptyCell), (0, EmptyCell), (1, EmptyCell)]
      , [(-2, EmptyCell), (-1, pc), (0, EmptyCell), (1, EmptyCell), (2, pc), (3, EmptyCell)]
      , [(-1, EmptyCell), (0, EmptyCell), (1, pc), (2, EmptyCell), (3, pc), (4, EmptyCell)]
      ]
    genPosCheck :: [[(Int, Cell)]] -> Coord -> Coord -> [([(Int, Int, Cell)], (Int, Int))]
    genPosCheck msk (cx, cy) (dx, dy) = map (\r -> (map (\(k, c) -> (cx + dx * k, cy + dy * k, c)) r, (dx, dy))) msk

delDoubleThree :: [[Cell]] -> Player -> [[Bool]] -> [[Bool]]
delDoubleThree grd p grd_dist =
  let (mw, mn) = mapMemoDoubleThree
      toCheck =
        if p == PlayerWhite
          then mw
          else mn
   in [[grd_dist !! y !! x && checkAllPos grd (toCheck !! y !! x) | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]
  where
    checkAllPos :: [[Cell]] -> [([(Int, Int, Cell)], (Int, Int))] -> Bool
    checkAllPos grida lpos =
      let tmp = map snd $ filter (checkLPos grida) lpos
          dDir = foldl' delDir [] tmp
       in 1 >= length dDir
    checkLPos :: [[Cell]] -> ([(Int, Int, Cell)], (Int, Int)) -> Bool
    checkLPos grd' (lp, _) = length lp == length (filter (checkPos grd') lp)
    checkPos :: [[Cell]] -> (Int, Int, Cell) -> Bool
    checkPos grid (x, y, pc) = x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && grid !! y !! x == pc
    delDir :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    delDir acc (drx, dry) = filter (\(dx, dy) -> not (drx == negate dx && dry == negate dy)) $ acc ++ [(drx, dry)]

-- True if is dist <= maxDist
distEmptyCellMap :: [[Cell]] -> Int -> [[Bool]]
distEmptyCellMap grille maxDist =
  let initMap = map (map (== EmptyCell)) grille
      iterator = [1 .. maxDist]
   in map (map not) $ foldl' (\b _ -> addDist1 b) initMap iterator
  where
    addDist1 :: [[Bool]] -> [[Bool]]
    addDist1 grid =
      [[grid !! y !! x && not (checkNeighbour grid x y) | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]
    checkNeighbour :: [[Bool]] -> Int -> Int -> Bool
    checkNeighbour grd x y =
      checkPos grd (x + 1) y ||
      checkPos grd x (y + 1) ||
      checkPos grd x (y - 1) ||
      checkPos grd (x - 1) y ||
      checkPos grd (x + 1) (y + 1) ||
      checkPos grd (x + 1) (y - 1) || checkPos grd (x - 1) (y + 1) || checkPos grd (x - 1) (y - 1)
    checkPos :: [[Bool]] -> Int -> Int -> Bool
    checkPos gd x y = x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && not (gd !! y !! x)

-- /!\ no valide play if the map is Empty!
validIACoords :: [[Cell]] -> Player -> Int -> [[Bool]]
validIACoords grd p d =
  let empty = map (map (== EmptyCell)) grd
      grd_dist = distEmptyCellMap grd d
      emptyAndDist = [[empty !! y !! x && grd_dist !! y !! x | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]
      v = delDoubleThree grd p emptyAndDist
   in v

sumTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTuples tupA tupB = (fst tupA + fst tupB, snd tupA + snd tupB)

------------
-- SOLVER --
------------
solver :: [[Cell]] -> Player -> Int -> Int -> Coord
solver = miniWrapper

countDir :: [[Cell]] -> Player -> Coord -> (Int, Int) -> Int
countDir grid player (cx, cy) (dx, dy) =
  let (_, nb) = foldl' sumDist (True, 0) [1 .. 4]
   in nb
  where
    sumDist (b, nb) d =
      let (cx', cy') = (cx + d * dx, cy + d * dy)
       in if b && 0 <= cx' && 0 <= cy' && hGoGrid > cx' && hGoGrid > cy' && grid !! cy' !! cx' == playerToPiece player
            then (True, nb + 1)
            else (False, nb)

moveScoring :: [[Cell]] -> Int -> Int -> Player -> Coord -> (Int, Int, Int)
moveScoring grid capWhite capBlack player move =
  let countedDir = map (countDir grid player move) allDir
      sumSameDir = map (\(c1, c2) -> (countedDir !! c1) + (countedDir !! c2) + 1) [(0, 5), (1, 4), (2, 3), (6, 7)]
      newCap = 2 * length (checkCapturToSup player move grid)
      nbCap =
        if player == PlayerWhite
          then capWhite + newCap
          else capBlack + newCap
      scoreCapture =
        if nbCap == 10
          then 1000000
          else 10000 * nbCap
      score = scoreCapture + foldl' transformToScore 0 sumSameDir
   in if player == PlayerWhite
        then (score, nbCap, capBlack)
        else (score, capWhite, nbCap)
  where
    countToScore count
      | count == 0 = 0
      | count == 1 = 1
      | count == 2 = 10
      | count == 3 = 100
      | count == 4 = 1000
      | otherwise = 1000000
    transformToScore :: Int -> Int -> Int
    transformToScore precSco count = precSco + countToScore count

--- if int < 0
moreThanOne :: [[Cell]] -> Coord -> Int -> (Int, Int) -> Int
moreThanOne grid move count direction
  | count > 1 = 2
  | 0 > fst move || 0 > snd move = count
  | (hGoGrid - 1) < fst move || (hGoGrid - 1) < snd move = count
  | EmptyCell == gridPiece = count
  | otherwise = moreThanOne grid (sumTuples move direction) (count + 1) direction
  where
    gridPiece = grid !! snd move !! fst move

worthMoveIA :: [[Cell]] -> Coord -> Bool
worthMoveIA grid move = True `elem` render
  where
    dirCouples = [(0, 5), (1, 4), (2, 3), (6, 7)]
    removeCoordIA :: [[Cell]] -> Coord -> (Int, Int) -> Int
    removeCoordIA grd mv direction = moreThanOne grd (sumTuples mv direction) 0 direction
    sumDir = map (removeCoordIA grid move) allDir
    render = [isTrue >= 2 | x <- dirCouples, let isTrue = (sumDir !! fst x) + (sumDir !! snd x)]

nextMoves :: [[Cell]] -> Player -> [Coord]
nextMoves grid player =
  let moves = validCoordToList $ validIACoords grid player 1
      optiMoves = filter (worthMoveIA grid) moves
   in if length optiMoves < 4
        then if null moves
               then validCoordToList $ validCoords grid player
               else moves
        else optiMoves

negaMax :: [[Cell]] -> Player -> Int -> Int -> Int -> Int -> Int -> Int
negaMax grid player depth alpha beta capWhite capBlack =
  let moves = nextMoves grid player
      nxtMovesAndScore :: [(Coord, (Int, Int, Int))]
      nxtMovesAndScore = map (\(cx, cy) -> ((cx, cy), moveScoring grid capWhite capBlack player (cx, cy))) moves
      movesSort = sortBy compF nxtMovesAndScore
      abPruning a ((cx, cy), (prSc, nW, nB)) =
        if a >= beta
          then a
          else let newGrid = posePieceAndDelete (cx, cy) player grid
                   resNega =
                     if prSc < 500000
                       then prSc - negaMax newGrid (nextPlayer player) (depth - 1) (-beta) (-a) nW nB
                       else prSc
                   newAlpha = max a resNega
                in newAlpha
      res =
        if depth > 0
          then foldl' abPruning alpha movesSort
          else maximum $ map (\(_, (s, _, _)) -> s) movesSort
   in res
  where
    compF :: (Coord, (Int, Int, Int)) -> (Coord, (Int, Int, Int)) -> Ordering
    compF (_, (s1, _, _)) (_, (s2, _, _))
      | s1 > s2 = LT
      | s1 < s2 = GT
      | otherwise = EQ

miniWrapper :: [[Cell]] -> Player -> Int -> Int -> Coord
miniWrapper grid player capWhite capBlack =
  let depth = 2 -- In reality depth = depth + 2
      alpha = div (minBound :: Int) 8
      beta = div (maxBound :: Int) 8
      moves = nextMoves grid player
      nxtMovesAndScore :: [(Coord, (Int, Int, Int))]
      nxtMovesAndScore = map (\(cx, cy) -> ((cx, cy), moveScoring grid capWhite capBlack player (cx, cy))) moves
      movesSort = sortBy compF nxtMovesAndScore
      abPruning (a, co) ((cx, cy), (prSc, nW, nB)) =
        if a >= beta
          then (a, co)
          else let newGrid = posePieceAndDelete (cx, cy) player grid
                   resNega =
                     if prSc < 500000
                       then prSc - negaMax newGrid (nextPlayer player) depth (-beta) (-a) nW nB
                       else prSc
                in if resNega > a
                     then (resNega, (cx, cy))
                     else (a, co)
      (_, bestMove) = foldl' abPruning (alpha, (-1, -1)) movesSort
   in bestMove
  where
    compF :: (Coord, (Int, Int, Int)) -> (Coord, (Int, Int, Int)) -> Ordering
    compF (_, (s1, _, _)) (_, (s2, _, _))
      | s1 > s2 = LT
      | s1 < s2 = GT
      | otherwise = EQ
