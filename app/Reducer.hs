module Reducer where

import Constant (allDir, hGoGrid)
import Control.Lens.Combinators (imap)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isJust, isNothing)
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
  let mCoord = solver (goGrid s) (playerTurn s) (nbPieceCapPBlack s) (nbPieceCapPWhite s)
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 9)
  let withDiff = s {lastIATimeForPlay = diff}
  return (handelPlayCoord mCoord withDiff)

suggestionPlay :: AppState -> IO AppState
suggestionPlay s =
  if isJust (end s)
    then return s
    else do
      start <- getCPUTime
      let coord = solver (goGrid s) (playerTurn s) (nbPieceCapPBlack s) (nbPieceCapPWhite s)
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10 ^ 9)
      return s {lastIATimeForPlay = diff, cursorSuggestion = Just (coord)}

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
solver :: [[Cell]] -> Player -> Int -> Int -> Coord
solver grd p nbCapBlack nbCapWihte =
  let scoreBlack = ([0, 0, 0, 0, 0], [0, 0, 0, 0, 0])
      scoreWhite = ([0, 0, 0, 0, 0], [0, 0, 0, 0, 0])
   in miniWrapper grd p 3 scoreWhite scoreBlack

countDirection :: [[Cell]] -> Player -> Coord -> Int -> (Int, Int) -> Int
countDirection grid player move count direction
  | 0 > fst move || 0 > snd move = count
  | (hGoGrid - 1) < fst move || (hGoGrid - 1) < snd move = count
  | playerPiece /= gridPiece = count
  | otherwise = countDirection grid player (sumTuples move direction) (count + 1) direction
  where
    playerPiece = playerToPiece player
    gridPiece = grid !! snd move !! fst move
    sumTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
    sumTuples tupA tupB = (fst tupA + fst tupB, snd tupA + snd tupB)

addNoSpaceScoring :: Int -> ([Int], [Int]) -> ([Int], [Int])
addNoSpaceScoring addSpace ([a, b, c, d, e], [f, g, h, i, j]) = (new, [f, g, h, i, j])
  where
    new
      | addSpace == 1 = [a + 1, b, c, d, e]
      | addSpace == 2 = [a, b + 1, c, d, e]
      | addSpace == 3 = [a, b, c + 1, d, e]
      | addSpace == 4 = [a, b, c, d + 1, e]
      | addSpace >= 5 = [a, b, c, d, e + 1]
      | otherwise = [a, b, c, d, e]

addSpaceScoring :: Int -> ([Int], [Int]) -> ([Int], [Int])
addSpaceScoring addSpace ([f, g, h, i, j], [a, b, c, d, e]) = ([f, g, h, i, j], new)
  where
    new
      | addSpace == 1 = [a + 1, b, c, d, e]
      | addSpace == 2 = [a, b + 1, c, d, e]
      | addSpace == 3 = [a, b, c + 1, d, e]
      | addSpace == 4 = [a, b, c, d + 1, e]
      | addSpace >= 5 = [a, b, c, d, e + 1]
      | otherwise = [a, b, c, d, e]

preScoringOneDirection :: [Int] -> (Int, Int) -> Int -> Int -> ([Int], [Int]) -> ([Int], [Int])
preScoringOneDirection countedDir dir space0 space5 exScoring
  -- 1
  | and [((countedDir !! (fst dir)) > 1), ((countedDir !! (snd dir)) > 1)] =
    addNoSpaceScoring ((countedDir !! (fst dir)) + (countedDir !! (snd dir)) - 1) exScoring
  -- 3 - 1
  | and [((countedDir !! (fst dir)) == 1), ((countedDir !! (snd dir)) > 1), (space0 == 0)] =
    addNoSpaceScoring ((countedDir !! (fst dir)) + (countedDir !! (snd dir)) - 1) exScoring
  -- 3 - 2
  | and [((countedDir !! (fst dir)) > 1), ((countedDir !! (snd dir)) == 1), (space5 == 0)] =
    addNoSpaceScoring ((countedDir !! (fst dir)) + (countedDir !! (snd dir)) - 1) exScoring
  | otherwise = scoringOneD'
  where
    scoringOneD'
        -- 5
      | and [((countedDir !! (fst dir)) == 1), ((countedDir !! (snd dir)) == 1), (space0 > 0), (space5 > 0)] =
        addSpaceScoring (space0 + 1) (addSpaceScoring (space5 + 1) exScoring)
        -- 4 - 1
      | and [((countedDir !! (fst dir)) == 1), ((countedDir !! (snd dir)) == 1), (space0 >= 1)] =
        addSpaceScoring (space0 + 1) exScoring
        -- 4 - 2
      | and [((countedDir !! (fst dir)) == 1), ((countedDir !! (snd dir)) == 1), (space5 >= 1)] =
        addSpaceScoring (space5 + 1) exScoring
        -- 6 - 1
      | and [((countedDir !! (fst dir)) > 1), ((countedDir !! (snd dir)) == 1), (space5 >= 1)] =
        addSpaceScoring ((countedDir !! (fst dir)) + space5) (addNoSpaceScoring (countedDir !! (fst dir)) exScoring)
        -- 6 - 2
      | and [((countedDir !! (fst dir)) == 1), ((countedDir !! (snd dir)) > 1), (space0 >= 1)] =
        addSpaceScoring ((countedDir !! (snd dir)) + space0) (addNoSpaceScoring (countedDir !! (snd dir)) exScoring)
        -- 2
      | otherwise = addNoSpaceScoring 1 exScoring

preScoring :: [[Cell]] -> Player -> Coord -> ([Int], [Int]) -> ([Int], [Int])
preScoring grid player move scoring = scoring5
  where
    spacePos :: (Int, Int) -> (Int, Int) -> (Int, Int)
    spacePos tupA tupB = (fst tupA + 2 * (fst tupB), snd tupA + 2 * (snd tupB))
    countSpace :: [[Cell]] -> [(Int, Int)] -> (Int, Int) -> Int -> Int
    countSpace grid allDir move idx
      | (fst move + fst (allDir !! idx)) < 0 || (snd move + snd (allDir !! idx)) < 0 = 0
      | (fst move + fst (allDir !! idx)) > (hGoGrid - 1) || (snd move + snd (allDir !! idx)) > (hGoGrid - 1) = 0
      | grid !! (snd move + snd (allDir !! idx)) !! (fst move + fst (allDir !! idx)) == EmptyCell =
        countDirection grid player (spacePos move (allDir !! idx)) 0 (allDir !! idx)
      | otherwise = 0
    addCaptureScoring :: ([Int], [Int]) -> Int -> ([Int], [Int])
    addCaptureScoring ([f, g, h, i, j], [a, b, c, d, e]) nb = ([f, g, h, i, j], [a + nb, b, c, d, e])
    -- (0, 1), (1, 0), (1, 1), (-1, -1), (-1, 0), (0, -1), (-1, 1), (1, -1)
    dirCouples = [(0, 5), (1, 4), (2, 3), (6, 7)]
    -- countDirection grid player move count direction
    countedDir = map (countDirection grid player move 0) allDir
    spaceCount = map (countSpace grid allDir move) [0 .. 7]
    -- preScoringOneDirection countedDir dir space0 space5 exScoring
    scoringPos1 = preScoringOneDirection countedDir (dirCouples !! 0) (spaceCount !! 0) (spaceCount !! 5) scoring
    scoringPos2 = preScoringOneDirection countedDir (dirCouples !! 1) (spaceCount !! 1) (spaceCount !! 4) scoringPos1
    scoringPos3 = preScoringOneDirection countedDir (dirCouples !! 2) (spaceCount !! 2) (spaceCount !! 3) scoringPos2
    scoringPos4 = preScoringOneDirection countedDir (dirCouples !! 3) (spaceCount !! 6) (spaceCount !! 7) scoringPos3
    scoring5 = addCaptureScoring scoringPos4 $ length (checkCapturToSup player move grid)

scoringCalc :: ([Int], [Int]) -> Integer
scoringCalc scoring =
  a * score1 + b * score2 + c * score3 + d * score4 + e * score5 + g * (score2 - 1) + h * (score3 - 1) +
  i * (score4 - 1) +
  j * (score5' - 1)
  where
    ([a, b, c, d, e], [f, g, h, i, j]) = (map toInteger (fst scoring), map toInteger (snd scoring))
    score1 = 1
    score2 = 100
    score3 = 1000
    score4 = 100000
    score5 = 10 * (toInteger (maxBound :: Int))
    score5' = (toInteger (maxBound :: Int)) - 1
    scoreTaken = 10 * (toInteger (maxBound :: Int))

--- diffScore blanc - noir
--- blanc => maximiser la différence => tendre vers +infini
--- noir => minimiser la différence => tendre vers - infini
miniMax :: [[Cell]] -> Player -> Int -> Integer -> Integer -> ([Int], [Int]) -> ([Int], [Int]) -> Coord -> Integer
miniMax grid player depth alpha beta whiteSco blackSco move
  | depth == 0 = diffScore
  | player == PlayerWhite && (length nxtMoveWhite) == 0 = diffScore
  | player == PlayerBlack && (length nxtMoveBlack) == 0 = diffScore
  | player == PlayerWhite = minimum outBlack
  | otherwise = maximum outWhite
  where
    newGrid' = posePiece move player grid
    newWhiteSco =
      if player == PlayerWhite
        then preScoring newGrid' PlayerWhite move whiteSco
        else whiteSco
    newBlackSco =
      if player == PlayerBlack
        then preScoring newGrid' PlayerBlack move blackSco
        else blackSco
    newGrid = posePieceAndDelete move player grid
    diffScore = (scoringCalc newWhiteSco) - (scoringCalc newBlackSco)
    nxtMoveWhite = validCoordToList (validIACoords newGrid PlayerWhite 1)
    nxtMoveBlack = validCoordToList (validIACoords newGrid PlayerBlack 1)
    miniMaxMap :: Integer -> [Coord] -> [Integer]
    miniMaxMap alph [] = []
    miniMaxMap alph (x:xs) = execMini : miniMaxMap alpha' newXs
      where
        execMini = miniMax newGrid PlayerWhite (depth - 1) alph beta newWhiteSco newBlackSco x
        alpha' = max alph execMini
        newXs =
          if beta <= alpha'
            then []
            else xs
    miniMaxMap2 :: Integer -> [Coord] -> [Integer]
    miniMaxMap2 bet [] = []
    miniMaxMap2 bet (x:xs) = execMini : miniMaxMap2 beta' newXs
      where
        execMini = miniMax newGrid PlayerBlack (depth - 1) alpha bet newWhiteSco newBlackSco x
        beta' = min bet execMini
        newXs =
          if beta' <= alpha
            then []
            else xs
    outWhite = miniMaxMap alpha nxtMoveWhite
    outBlack = miniMaxMap2 beta nxtMoveBlack

miniWrapper :: [[Cell]] -> Player -> Int -> ([Int], [Int]) -> ([Int], [Int]) -> Coord
miniWrapper grid player depth whiteSco blackSco
  | player == PlayerBlack = nxtMoveWhite !! whiteRet
  | otherwise = nxtMoveBlack !! blackRet
  where
    nxtMoveWhite = validCoordToList (validIACoords newGrid PlayerWhite 1)
    nxtMoveBlack = validCoordToList (validIACoords newGrid PlayerBlack 1)
    alpha = (8 * (toInteger (minBound :: Int)))
    beta = (8 * (toInteger (maxBound :: Int)))
    miniMaxMap :: Integer -> [Coord] -> [Integer]
    miniMaxMap alph [] = []
    miniMaxMap alph (x:xs) = execMini : miniMaxMap alpha' newXs
      where
        execMini = miniMax newGrid PlayerWhite depth alph beta newWhiteSco newBlackSco x
        alpha' = max alph execMini
        newXs =
          if beta <= alpha'
            then []
            else xs
    miniMaxMap2 :: Integer -> [Coord] -> [Integer]
    miniMaxMap2 bet [] = []
    miniMaxMap2 bet (x:xs) = execMini : miniMaxMap2 beta' newXs
      where
        execMini = miniMax newGrid PlayerBlack depth alpha bet newWhiteSco newBlackSco x
        beta' = min bet execMini
        newXs =
          if beta' <= alpha
            then []
            else xs
    outWhite = miniMaxMap alpha nxtMoveWhite
    outBlack = miniMaxMap2 beta nxtMoveBlack
    whiteRet = fromMaybe 0 (elemIndex (maximum outWhite) outWhite)
    blackRet = fromMaybe 0 (elemIndex (minimum outWhite) outBlack)
