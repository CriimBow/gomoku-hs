module Solver where

import Reducer
import Constant (allDir, hGoGrid)
import Data.List (elemIndex)
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)

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


addNoSpaceScoring ::  Int -> ([Int], [Int]) -> ([Int], [Int])
addNoSpaceScoring addSpace ([a,b,c,d,e], [f,g,h,i,j]) = (new, [f,g,h,i,j])
  where
    new
      | addSpace == 1 = [a+1,b,c,d,e]
      | addSpace == 2 = [a,b+1,c,d,e]
      | addSpace == 3 = [a,b,c+1,d,e]
      | addSpace == 4 = [a,b,c,d+1,e]
      | addSpace >= 5 = [a,b,c,d,e+1]
      | otherwise = [a,b,c,d,e]


addSpaceScoring :: Int -> ([Int], [Int]) -> ([Int], [Int])
addSpaceScoring addSpace ([f,g,h,i,j],[a,b,c,d,e]) = ([f,g,h,i,j],new)
  where
    new
      | addSpace == 1 = [a+1,b,c,d,e]
      | addSpace == 2 = [a,b+1,c,d,e]
      | addSpace == 3 = [a,b,c+1,d,e]
      | addSpace == 4 = [a,b,c,d+1,e]
      | addSpace >= 5 = [a,b,c,d,e+1]
      | otherwise = [a,b,c,d,e]


preScoringOneDirection :: [Int] -> (Int, Int) -> Int -> Int -> ([Int], [Int]) -> ([Int], [Int])
preScoringOneDirection countedDir dir space0 space5 exScoring
  -- 1
  | and [((countedDir !! (fst dir)) > 1), ((countedDir !! (snd dir)) > 1)] = addNoSpaceScoring ((countedDir !! (fst dir)) + (countedDir !! (snd dir)) - 1) exScoring
  -- 3 - 1
  | and [((countedDir !! (fst dir)) == 1), ((countedDir !! (snd dir)) > 1), (space0 == 0)] = addNoSpaceScoring ((countedDir !! (fst dir)) + (countedDir !! (snd dir)) - 1) exScoring
  -- 3 - 2
  | and [((countedDir !! (fst dir)) > 1), ((countedDir !! (snd dir)) == 1), (space5 == 0)] = addNoSpaceScoring ((countedDir !! (fst dir)) + (countedDir !! (snd dir)) - 1) exScoring
  | otherwise = scoringOneD'
  where
    scoringOneD'
        -- 5
        | and [((countedDir !! (fst dir)) == 1), ((countedDir !! (snd dir)) == 1), (space0 > 0), (space5 > 0)] = addSpaceScoring (space0 + 1) (addSpaceScoring (space5 + 1) exScoring)
        -- 4 - 1
        | and [((countedDir !! (fst dir)) == 1), ((countedDir !! (snd dir)) == 1), (space0 >= 1)] = addSpaceScoring (space0 + 1) exScoring
        -- 4 - 2
        | and [((countedDir !! (fst dir)) == 1), ((countedDir !! (snd dir)) == 1), (space5 >= 1)] = addSpaceScoring (space5 + 1) exScoring
        -- 6 - 1
        | and [((countedDir !! (fst dir)) > 1), ((countedDir !! (snd dir)) == 1), (space5 >= 1)] = addSpaceScoring ((countedDir !! (fst dir)) + space5) (addNoSpaceScoring (countedDir !! (fst dir)) exScoring)
        -- 6 - 2
        | and [((countedDir !! (fst dir)) == 1), ((countedDir !! (snd dir)) > 1), (space0 >= 1)] = addSpaceScoring ((countedDir !! (snd dir)) + space0) (addNoSpaceScoring (countedDir !! (snd dir)) exScoring)
        -- 2
        | otherwise = addNoSpaceScoring 1 exScoring


preScoring :: [[Cell]] -> Player -> Coord -> ([Int], [Int]) -> ([Int], [Int])
preScoring grid player move scoring =
  scoringPos4
  where
    spacePos :: (Int, Int) -> (Int, Int) -> (Int, Int)
    spacePos tupA tupB = (fst tupA + 2 * (fst tupB), snd tupA + 2 * (snd tupB))

    countSpace :: [[Cell]] -> [(Int, Int)] -> (Int,Int) -> Int -> Int
    countSpace grid allDir move idx
        | (fst move + fst (allDir !! idx)) < 0 || (snd move + snd (allDir !! idx)) < 0 = 0
        | (fst move + fst (allDir !! idx)) > (hGoGrid - 1) || (snd move + snd (allDir !! idx)) > (hGoGrid - 1) = 0
        | grid !! (snd move + snd (allDir !! idx)) !! (fst move + fst (allDir !! idx)) == EmptyCell = countDirection grid player (spacePos move (allDir !! idx)) 0 (allDir !! idx)
        | otherwise = 0

    -- (0, 1), (1, 0), (1, 1), (-1, -1), (-1, 0), (0, -1), (-1, 1), (1, -1)
    dirCouples = [(0, 5), (1, 4), (2, 3), (6, 7)]
    -- countDirection grid player move count direction
    countedDir = map (countDirection grid player move 0) allDir
    spaceCount = map (countSpace grid allDir move) [0..7]

    -- preScoringOneDirection countedDir dir space0 space5 exScoring
    scoringPos1 = preScoringOneDirection countedDir (dirCouples !! 0) (spaceCount !! 0) (spaceCount !! 5) scoring
    scoringPos2 = preScoringOneDirection countedDir (dirCouples !! 1) (spaceCount !! 1) (spaceCount !! 4) scoringPos1
    scoringPos3 = preScoringOneDirection countedDir (dirCouples !! 2) (spaceCount !! 2) (spaceCount !! 3) scoringPos2
    scoringPos4 = preScoringOneDirection countedDir (dirCouples !! 3) (spaceCount !! 6) (spaceCount !! 7) scoringPos3
    
    
scoringCalc :: ([Int], [Int]) -> Integer
scoringCalc scoring =
  a * score1 + b * score2 + c * score3 + d * score4 + e * score5 + g * (score2 - 1) + h * (score3 - 1) + i * (score4 - 1) + j * (score5' - 1)
  where
    ([a,b,c,d,e], [f,g,h,i,j]) = (map toInteger (fst scoring), map toInteger (snd scoring))
    score1 = 1
    score2 = 100
    score3 = 1000
    score4 = 100000
    score5 = 10 *(toInteger (maxBound :: Int))
    score5' = (toInteger (maxBound :: Int)) - 1

--- diffScore blanc - noir
--- blanc => maximiser la différence => tendre vers +infini
--- noir => minimiser la différence => tendre vers - infini

miniMax :: [[Cell]] -> Player -> Int -> Integer -> Integer -> ([Int], [Int]) -> ([Int], [Int]) -> Coord -> (Integer, Coord)
miniMax grid player depth alpha beta whiteSco blackSco move
  | depth == 0 = (diffScore, move)
  | player == PlayerWhite && (length nxtMoveWhite) == 0 = (diffScore, move)
  | player == PlayerBlack && (length nxtMoveBlack) == 0 = (diffScore, move)
  --- | depth == (initDepth - 1) && player == PlayerWhite = ((maximum (returnScores outBlack), move))
  --- | depth == (initDepth - 1) && player == PlayerBlack = ((minimum (returnScores outWhite), move))
  | player == PlayerWhite = outBlack !! blackIdx
  | otherwise = outWhite !! whiteIdx
  where
    returnScores :: [(Integer, Coord)] -> [Integer]
    returnScores out = [fst tuple | x <- [0 .. (length out - 1)], let tuple = out !! x]

    newGrid = posePiece move player grid

    newWhiteSco = if player == PlayerWhite
                    then preScoring newGrid PlayerWhite move whiteSco
                    else whiteSco
    newBlackSco = if player == PlayerBlack
                    then preScoring newGrid PlayerBlack move blackSco
                    else blackSco

    diffScore = (scoringCalc newWhiteSco) - (scoringCalc newBlackSco)
    nxtMoveWhite = validCoordToList (validIACoords newGrid PlayerWhite 1)
    nxtMoveBlack = validCoordToList (validIACoords newGrid PlayerBlack 1)

    miniMaxMap :: Integer -> [Coord] -> [(Integer, Coord)]
    miniMaxMap alph [] = []
    miniMaxMap alph (x:xs) =
       execMini : miniMaxMap alpha' newXs
      where
        execMini = miniMax newGrid PlayerWhite (depth - 1) alph beta newWhiteSco newBlackSco x
        alpha' = max alph (fst execMini)
        newXs = if beta <= alpha'
                then []
                else xs

    miniMaxMap2 :: Integer -> [Coord] -> [(Integer, Coord)]
    miniMaxMap2 bet [] = []
    miniMaxMap2 bet (x:xs) =
       execMini : miniMaxMap2 beta' newXs
      where
        execMini = miniMax newGrid PlayerBlack (depth - 1) alpha bet newWhiteSco newBlackSco x
        beta' = min bet (fst execMini)
        newXs = if beta' <= alpha
                then []
                else xs

    outWhite = miniMaxMap alpha nxtMoveWhite
    -- [(123456, (0,0)), (1236, (0,2)), (14236, (1,2))]
    outBlack = miniMaxMap2 beta nxtMoveBlack

    whiteIdx = fromMaybe 0 (elemIndex (maximum (returnScores outWhite)) (returnScores outWhite))
    blackIdx = fromMaybe 0 (elemIndex (minimum (returnScores outBlack)) (returnScores outBlack))


bestWrapper :: [[Cell]] -> Player -> Int -> ([Int], [Int]) -> ([Int], [Int]) -> Coord -> Coord
bestWrapper grid player depth whiteSco blackSco move
 | player == PlayerBlack = nxtMoveWhite !! whiteRet
 | otherwise = nxtMoveBlack !! blackRet
 where
   returnScores :: [(Integer, Coord)] -> [Integer]
   returnScores out = [fst tuple | x <- [0 .. (length out - 1)], let tuple = out !! x]
   newGrid = posePiece move player grid
   newWhiteSco = if player == PlayerWhite
                   then preScoring newGrid PlayerWhite move whiteSco
                   else whiteSco
   newBlackSco = if player == PlayerBlack
                   then preScoring newGrid PlayerBlack move blackSco
                   else blackSco

   nxtMoveWhite = validCoordToList (validIACoords newGrid PlayerWhite 2)
   nxtMoveBlack = validCoordToList (validIACoords newGrid PlayerBlack 2)

   alpha = (8 *(toInteger (minBound :: Int)))
   beta = (8 *(toInteger (maxBound :: Int)))

   miniMaxMap :: Integer -> [Coord] -> [(Integer, Coord)]
   miniMaxMap alph [] = []
   miniMaxMap alph (x:xs) =
      execMini : miniMaxMap alpha' newXs
     where
       execMini = miniMax newGrid PlayerWhite depth alph beta newWhiteSco newBlackSco x
       alpha' = max alph (fst execMini)
       newXs = if beta <= alpha'
               then []
               else xs

   miniMaxMap2 :: Integer -> [Coord] -> [(Integer, Coord)]
   miniMaxMap2 bet [] = []
   miniMaxMap2 bet (x:xs) =
      execMini : miniMaxMap2 beta' newXs
     where
       execMini = miniMax newGrid PlayerBlack depth alpha bet newWhiteSco newBlackSco x
       beta' = min bet (fst execMini)
       newXs = if beta' <= alpha
               then []
               else xs

   outWhite = miniMaxMap alpha nxtMoveWhite
   outBlack = miniMaxMap2 beta nxtMoveBlack

   whiteRet = fromMaybe 0 (elemIndex (maximum (returnScores outWhite)) (returnScores outWhite))
   blackRet = fromMaybe 0 (elemIndex (minimum (returnScores outWhite)) (returnScores outBlack))

