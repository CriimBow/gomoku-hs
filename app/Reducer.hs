module Reducer where

import Constant (allDir, hGoGrid)
import Control.DeepSeq
import Control.Lens.Combinators (imap)

-- import Control.Parallel (par)
-- import Data.List (elemIndex)
-- import Data.List.Split (chunksOf)
import Data.Maybe (isJust)

import Debug.Trace (trace, traceIO, traceShow)
import System.CPUTime

-- import System.IO
-- import System.Random (Random(..), newStdGen)
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
          _ -> [[EmptyCell | _ <- [1 .. hGoGrid]] | _ <- [1 .. hGoGrid]]
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
    genPosCheck (cx, cy) player (dx, dy) =
      [ (cx + dx, cy + dy, nextPlayer player)
      , (cx + dx * 2, cy + dy * 2, nextPlayer player)
      , (cx + dx * 3, cy + dy * 3, player)
      ]
    checkPoss grid psCks = length (filter (checkPos grid) psCks) == 3
    checkPos gd (x, y, plr) = x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && gd !! y !! x == playerToPiece plr

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

-- can use delDoubleThree
validCoords :: [[Cell]] -> Player -> [[Bool]]
validCoords grd p = delDoubleThree grd p (map (map (== EmptyCell)) grd)

validCoord :: [[Cell]] -> Player -> Coord -> Bool
validCoord grd p (cx, cy) = cx >= 0 && cx < hGoGrid && cy >= 0 && cy < hGoGrid && validCoords grd p !! cy !! cx

validCoordToList :: [[Bool]] -> [(Int, Int)]
validCoordToList grid = [(x, y) | x <- [0 .. hGoGrid - 1], y <- [0 .. hGoGrid - 1], grid !! y !! x]

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
delDoubleThree :: [[Cell]] -> Player -> [[Bool]] -> [[Bool]]
delDoubleThree grd p grd_dist =
  let msk = (maskCoef $ playerToPiece p)
   in [[grd_dist !! y !! x && checkDoubleThree grd msk (x, y) | x <- [0 .. hGoGrid - 1]] | y <- [0 .. hGoGrid - 1]]
  where
    maskCoef pc =
      [ [(-3, EmptyCell), (-2, pc), (-1, pc), (0, EmptyCell), (1, EmptyCell)]
      , [(-2, EmptyCell), (-1, pc), (0, EmptyCell), (1, pc), (2, EmptyCell)]
      , [(-4, EmptyCell), (-3, pc), (-2, pc), (-1, EmptyCell), (0, EmptyCell), (1, EmptyCell)]
      , [(-2, EmptyCell), (-1, pc), (0, EmptyCell), (1, EmptyCell), (2, pc), (3, EmptyCell)]
      , [(-1, EmptyCell), (0, EmptyCell), (1, pc), (2, EmptyCell), (3, pc), (4, EmptyCell)]
      ]
    checkDoubleThree grille msk cr = checkAllPos grille $ allDir >>= genPosCheck msk cr
    genPosCheck :: [[(Int, Cell)]] -> Coord -> Coord -> [([(Int, Int, Cell)], (Int, Int))]
    genPosCheck msk (cx, cy) (dx, dy) = map (\r -> (map (\(k, c) -> (cx + dx * k, cy + dy * k, c)) r, (dx, dy))) msk
    checkAllPos :: [[Cell]] -> [([(Int, Int, Cell)], (Int, Int))] -> Bool
    checkAllPos grida lpos =
      let tmp = map snd $ filter (checkLPos grida) lpos
          dDir = foldr delDir [] tmp
       in 1 >= length dDir
    checkLPos :: [[Cell]] -> ([(Int, Int, Cell)], (Int, Int)) -> Bool
    checkLPos grd' (lp, _) = length lp == length (filter (checkPos grd') lp)
    checkPos :: [[Cell]] -> (Int, Int, Cell) -> Bool
    checkPos grid (x, y, pc) = x >= 0 && x < hGoGrid && y >= 0 && y < hGoGrid && grid !! y !! x == pc
    delDir :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    delDir (drx, dry) acc = filter (\(dx, dy) -> not (drx == negate dx && dry == negate dy)) $ acc ++ [(drx, dry)]

-- True if is dist <= maxDist
distEmptyCellMap :: [[Cell]] -> Int -> [[Bool]]
distEmptyCellMap grille maxDist =
  let initMap = map (map (== EmptyCell)) grille
      iterator = [1 .. maxDist]
   in map (map not) $ foldr (\_ b -> addDist1 b) initMap iterator
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
solver grd p nbCapWihte nbCapBlack = miniWrapper grd p nbCapWihte nbCapBlack

{-
preScoring :: [[Cell]] -> Player -> Coord -> ([Int], [Int]) -> ([Int], [Int])
preScoring grid player move scoring = scoring5
  where
    spacePos :: (Int, Int) -> (Int, Int) -> (Int, Int)
    spacePos tupA tupB = (fst tupA + 2 * (fst tupB), snd tupA + 2 * (snd tupB))
    countSpace :: [[Cell]] -> (Int, Int) -> Int -> Int
    countSpace grid move idx
      | (fst move + fst (allDir !! idx)) < 0 || (snd move + snd (allDir !! idx)) < 0 = 0
      | (fst move + fst (allDir !! idx)) > (hGoGrid - 1) || (snd move + snd (allDir !! idx)) > (hGoGrid - 1) = 0
      | grid !! (snd move + snd (allDir !! idx)) !! (fst move + fst (allDir !! idx)) == EmptyCell =
        countDirection grid player (spacePos move (allDir !! idx)) 0 (allDir !! idx)
      | otherwise = 0
    addCaptureScoring :: ([Int], [Int]) -> Int -> ([Int], [Int])
    addCaptureScoring (f, [a, b, c, d, e]) nb = (f, [a + nb, b, c, d, e])
    -- (0, 1), (1, 0), (1, 1), (-1, -1), (-1, 0), (0, -1), (-1, 1), (1, -1)
    dirCouples = [(0, 5), (1, 4), (2, 3), (6, 7)]
    -- countDirection grid player move count direction
    countedDir = map (countDirection grid player move 0) allDir
    spaceCount = map (countSpace grid move) [0 .. 7]
    -- preScoringOneDirection countedDir dir space0 space5 exScoring
    scoringPos1 = preScoringOneDirection countedDir (dirCouples !! 0) (spaceCount !! 0) (spaceCount !! 5) scoring
    scoringPos2 = preScoringOneDirection countedDir (dirCouples !! 1) (spaceCount !! 1) (spaceCount !! 4) scoringPos1
    scoringPos3 = preScoringOneDirection countedDir (dirCouples !! 2) (spaceCount !! 2) (spaceCount !! 3) scoringPos2
    scoringPos4 = preScoringOneDirection countedDir (dirCouples !! 3) (spaceCount !! 6) (spaceCount !! 7) scoringPos3
    scoring5 = addCaptureScoring scoringPos4 $ length (checkCapturToSup player move grid)

-}
{-
countDirection :: [[Cell]] -> Player -> Coord -> Int -> (Int, Int) -> Int
countDirection grid player move count direction
  | 0 > fst move || 0 > snd move = count
  | (hGoGrid - 1) < fst move || (hGoGrid - 1) < snd move = count
  | playerPiece /= gridPiece = count
  | otherwise = countDirection grid player (sumTuples move direction) (count + 1) direction
  where
    playerPiece = playerToPiece player
    gridPiece = grid !! snd move !! fst move
-}
countDir :: [[Cell]] -> Player -> Coord -> (Int, Int) -> Int
countDir grid player (cx, cy) (dx, dy) =
  let (_, nb) = foldl sumDist (True, 0) [1 .. 4]
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
      score = scoreCapture + foldl transformToScore 0 sumSameDir
   in if player == PlayerWhite
        then (score, nbCap, capBlack)
        else (score, capWhite, nbCap)
  where
    countToScore count
      | count <= 1 = 0
      | count == 2 = 10
      | count == 3 = 100
      | count == 4 = 1000
      | otherwise = 100000
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

{-

scoringCalc :: ([Int], [Int]) -> Integer
scoringCalc scoring =
  a * score1 + b * score2 + c * score3 + d * score4 + e * score5 + g * (score2 - 1) + h * (score3 - 1) +
  i * (score4 - 1) +
  j * (score5' - 1) +
  scoreTaken
  where
    ([a, b, c, d, e], [f, g, h, i, j]) = (map toInteger (fst scoring), map toInteger (snd scoring))
    score1 = 1
    score2 = 100
    score3 = 1000
    score4 = 100000
    score5 = 10 * (toInteger (maxBound :: Int))
    score5' = (toInteger (maxBound :: Int)) - 1
    scoreTaken =
      if f == 5
        then 10 * (toInteger (maxBound :: Int)) + 1
        else 1000000 * f

--- diffScore blanc - noir
--- blanc => maximiser la différence => tendre vers +infini
--- noir => minimiser la différence => tendre vers - infini
miniMax :: [[Cell]] -> Player -> Int -> Int -> Int -> Int -> Int -> Int
miniMax grid player depth alpha beta capWhite capBlack
  | depth == 0 = diffScore
  | player == PlayerWhite && (length nxtMoveWhite) == 0 = diffScore
  | player == PlayerBlack && (length nxtMoveBlack) == 0 = diffScore
  | player == PlayerWhite = minimum outBlack
  | otherwise = maximum outWhite
  where
    newCapWhite =
      if player == PlayerWhite
        then capWhite + 2 * length (checkCapturToSup player move grid)
        else capWhite
    newCapBlack =
      if player == PlayerBlack
        then capBlack + 2 * length (checkCapturToSup player move grid)
        else capBlack
    diffScore = (scoringCalc newWhiteSco) - (scoringCalc newBlackSco)
    nxtMoveWhite =
      let moves = validCoordToList (validIACoords newGrid PlayerWhite 1)
          optiMoves = filter (worthMoveIA newGrid) moves
       in if null optiMoves
            then moves
            else optiMoves
    nxtMoveBlack =
      let moves = validCoordToList (validIACoords newGrid PlayerBlack 1)
          optiMoves = filter (worthMoveIA newGrid) moves
       in if null optiMoves
            then moves
            else optiMoves
    --- newGrid = posePieceAndDelete move player grid
    miniMaxMap :: Int -> [Coord] -> [Int]
    miniMaxMap _ [] = []
    miniMaxMap alph ((cx, cy):xs) = execMini : miniMaxMap alpha' newXs
      where
        execMini =
          miniMax (posePieceAndDelete (cx, cy) player grid) PlayerWhite (depth - 1) alph beta newCapWhite newCapBlack x
        alpha' = max alph execMini
        newXs =
          if beta <= alpha'
            then []
            else xs
    miniMaxMap2 :: Int -> [Coord] -> [Int]
    miniMaxMap2 _ [] = []
    miniMaxMap2 bet (x:xs) = execMini : miniMaxMap2 beta' newXs
      where
        execMini = miniMax newGrid PlayerBlack (depth - 1) alpha bet newCapWhite newCapBlack x
        beta' = min bet execMini
        newXs =
          if beta' <= alpha
            then []
            else xs
    outWhite = miniMaxMap alpha nxtMoveWhite
    outBlack = miniMaxMap2 beta nxtMoveBlack
    {-
    moveSoring :: [[Cell]] -> Int -> Int -> Player -> Coord -> Int
    moveSoring grid capWhite capBlack player move
    -}
    --- newGrid = posePieceAndDelete move player grid
-}
nextMoves :: [[Cell]] -> Player -> [Coord]
nextMoves grid player =
  let moves = validCoordToList $ validIACoords grid player 1
      optiMoves = filter (worthMoveIA grid) moves
   in if null optiMoves
        then if null moves
               then validCoordToList $ validCoords grid player
               else moves
        else optiMoves

negaMax :: [[Cell]] -> Player -> Int -> Int -> Int -> Int -> Int -> Int
negaMax grid player depth alpha beta capWhite capBlack =
  let moves = nextMoves grid player
      nxtMovesAndScore :: [(Coord, (Int, Int, Int))]
      nxtMovesAndScore = map (\(cx, cy) -> ((cx, cy), moveScoring grid capWhite capBlack player (cx, cy))) moves
    -- sort
      abPruning a ((cx, cy), (prSc, nW, nB)) =
        if a >= beta
          then a
          else let newGrid = posePieceAndDelete (cx, cy) (nextPlayer player) grid
                   resNega = prSc - negaMax newGrid (nextPlayer player) (depth - 1) (-beta) (-a) nW nB
                   newAlpha = max a resNega
                in newAlpha
      res =
        if depth > 0
          then foldl abPruning alpha nxtMovesAndScore
          else maximum $ map (\(_, (s, _, _)) -> s) nxtMovesAndScore
   in res

miniWrapper :: [[Cell]] -> Player -> Int -> Int -> Coord
miniWrapper grid player capWhite capBlack =
  let depth = 2 -- In reality depth = depth + 2
      moves = nextMoves grid player
      nxtMovesAndScore :: [(Coord, (Int, Int, Int))]
      nxtMovesAndScore = map (\(cx, cy) -> ((cx, cy), moveScoring grid capWhite capBlack player (cx, cy))) moves
      alpha = div (minBound :: Int) 8
      beta = div (maxBound :: Int) 8
    -- sort
      abPruning (a, co) ((cx, cy), (prSc, nW, nB)) =
        if a >= beta
          then (a, co)
          else let newGrid = posePieceAndDelete (cx, cy) (nextPlayer player) grid
                   resNega = prSc - negaMax newGrid (nextPlayer player) depth (-beta) (-a) nW nB
                in if resNega > a
                     then (resNega, (cx, cy))
                     else (a, co)
      (_, bestMove) = trace (show nxtMovesAndScore) (foldl abPruning (alpha, (-1, -1)) nxtMovesAndScore)
   in bestMove
{-

miniWrapper :: [[Cell]] -> Player -> Int -> Int -> Int -> Coord
miniWrapper grid player depth capWhite capBlack
  | player == PlayerBlack = nxtMoveWhite !! whiteRet
  | otherwise = nxtMoveBlack !! blackRet
  where
    nxtMoveWhite =
      let moves = validCoordToList (validIACoords grid PlayerWhite 1)
          optiMoves = filter (worthMoveIA grid) moves
       in if null optiMoves
            then moves
            else optiMoves
    nxtMoveBlack =
      let moves = validCoordToList (validIACoords grid PlayerBlack 1)
          optiMoves = filter (worthMoveIA grid) moves
       in if null optiMoves
            then moves
            else optiMoves
    alpha = minBound :: Int
    beta = maxBound :: Int
    miniMaxMap :: Int -> [Coord] -> [Int]
    miniMaxMap _ [] = []
    miniMaxMap alph (x:xs) = execMini : miniMaxMap alpha' newXs
      where
        execMini = miniMax grid PlayerWhite depth alph beta capWhite capBlack x
        alpha' = max alph execMini
        newXs =
          if beta <= alpha'
            then []
            else xs
    miniMaxMap2 :: Int -> [Coord] -> [Int]
    miniMaxMap2 _ [] = []
    miniMaxMap2 bet (x:xs) = execMini : miniMaxMap2 beta' newXs
      where
        execMini = miniMax grid PlayerBlack depth alpha bet capWhite capBlack x
        beta' = min bet execMini
        newXs =
          if beta' <= alpha
            then []
            else xs
    outWhite = miniMaxMap alpha nxtMoveWhite
    outBlack = miniMaxMap2 beta nxtMoveBlack
    whiteRet = fromMaybe 0 (elemIndex (maximum outWhite) outWhite)
    blackRet = fromMaybe 0 (elemIndex (minimum outBlack) outBlack)
-}