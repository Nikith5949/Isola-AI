module FlavioAndrade2 where

import Data.Char (ord)
import Data.Ord
import Data.List
import Data.List(nubBy)
import Data.Function
import System.IO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Timeout
import IsolaShared

getName = "Flavio Andrade2"

-- Returns the chosen move and its maximum search depth for a given state.
getMove :: State -> IO (Move, Int)

getMove state = timedDeepening state 1

-- Structure to hold a potential move, the board state after executing it, and the score for that state
data MoveInfo = MoveInfo {move :: Move, newState :: State, newScore :: Double}

-- Receives the current state and the initial maximum search depth.
-- Returns the chosen move and its maximum search depth.
timedDeepening :: State -> Int -> IO (Move, Int)
timedDeepening state initMaxDepth =
    do startTime <- getPOSIXTime
       deepen initMaxDepth nullMove startTime
       where deepen maxDepth bestMove sTime =
                 -- bind current time to currTime
                 do currTime <- getPOSIXTime
                    let remainTime = mcrsecPerMove - floor ((currTime - sTime) * 1000000)
                    -- findBestMove: finds best move for a given maxDepth
                    compMove <- timeout remainTime (return $! findBestMove state 0 (-winScore - 1) (winScore + 1) maxDepth)
                    loop compMove
                    where loop Nothing  = return (bestMove, maxDepth - 1)
                          loop (Just (move, score))
                              | abs score > winScore = return (move, maxDepth)
                              | otherwise            = deepen (maxDepth + 1) move sTime


-- findBestMove receives the current state, the best score found so far, current alpha, beta,
-- and the current number of levels that we are above maxDepth.
-- It uses Minimax and alpha-beta pruning to find the strongest move and return this move and its
-- associated score. Highested rated moves are tested first
findBestMove :: State -> Double -> Double -> Double -> Int -> (Move, Double)
findBestMove st staticScore alpha beta level
    | level == 0 || abs staticScore > winScore = (nullMove, staticScore)
    | pl1Turn st = lookAheadMax nullMove alpha sMoves
    | otherwise  = lookAheadMin nullMove beta sMoves
                   where sMoves = sortedMoves st
                         lookAheadMax topMove topScore [] = (topMove, topScore)
                         lookAheadMax topMove topScore (m:ms)
                             | scAlpha >= beta     = (move m, scAlpha + 1)
                             | scAlpha >= topScore = lookAheadMax (move m) scAlpha ms
                             | otherwise           = lookAheadMax topMove topScore ms
                             where scAlpha = snd $ findBestMove (newState m) (newScore m) topScore beta  (level - 1)
                         lookAheadMin topMove topScore [] = (topMove, topScore)
                         lookAheadMin topMove topScore (m:ms)
                             | scBeta <= alpha     = (move m, scBeta - 1)
                             | scBeta <= topScore  = lookAheadMin (move m) scBeta  ms
                             | otherwise           = lookAheadMin topMove topScore ms
                             where scBeta  = snd $ findBestMove (newState m) (newScore m) alpha topScore (level - 1)

-- For a given state, returns a sorted MoveInfo list in which the most favorable moves appear first.
-- This sorting is used by findBestMove to make alpha-beta pruning as effective as possible.
sortedMoves :: State -> [MoveInfo]
sortedMoves st = if pl1Turn st then reverse sortedList else sortedList
    where sortedList = sortBy (comparing newScore) moveList
          moveList = [MoveInfo {move = m, newState = nc, newScore = ns}
                         | m <- genMoves st, let nc = execMove st m, let ns = score nc]

-- If the absolute score exceeds winScore, it means that the game is over (saves extra check)
winScore = 999.0

-- score is the e(p) function, where p is the state. Player 1 is Max, Player 2 is Min.
score :: State -> Double
score st
    | m1 == 0 && m2 == 0 && pl1Turn st = -1000.0
    | m1 == 0 && m2 == 0               =  1000.0
    | m1 == 0                          = -1000.0
    | m2 == 0                          =  1000.0
    | otherwise                        = fromIntegral (m1 - m2)
    -- The idea is to look at which player has the most neighbors.
    -- This is done by getting the neighbors of the positions that you can jump
    -- to, and by looking at those neighbors neighbors. This is done for
    -- four levels.
    where m1Moves = (genMoves st {pl1Turn = True})
          m2Moves = (genMoves st {pl1Turn = False})
          neighborsM1 = [ neighbors'' (tiles st) x | x <- jumpToPositions m1Moves]
          neighborsM2 = [ neighbors'' (tiles st) x | x <- jumpToPositions m2Moves]
          neighborsM1' = if neighborsM1 == [] then [] else [ neighbors'' (tiles st) x | x <- head neighborsM1]
          neighborsM2' = if neighborsM2 == [] then [] else [ neighbors'' (tiles st) x | x <- head neighborsM2]
          neighborsM1'' = if neighborsM1' == [] then [] else [ neighbors'' (tiles st) x | x <- head neighborsM1']
          neighborsM2'' = if neighborsM2' == [] then [] else [ neighbors'' (tiles st) x | x <- head neighborsM2']

          removeNeighborsM1 =  [ neighbors'' (tiles st) x | x <- removePositions m1Moves]
          removeNeighborsM2 =  [ neighbors'' (tiles st) x | x <- removePositions m2Moves]
          removeNeighborsM1' =  if removeNeighborsM1 == [] then [] else [ neighbors'' (tiles st) x | x <- head removeNeighborsM1]
          removeNeighborsM2' =  if removeNeighborsM2 == [] then [] else [ neighbors'' (tiles st) x | x <- head removeNeighborsM2]
          --removeNeighborsM1'' =  if removeNeighborsM1 == [] then [] else [ neighbors'' (tiles st) x | x <- head removeNeighborsM1']
          --removeNeighborsM2'' =  if removeNeighborsM2 == [] then [] else [ neighbors'' (tiles st) x | x <- head removeNeighborsM2']

          neighbors1 = (length neighborsM1) + (length neighborsM1') + (length neighborsM1'') + (length m1Moves)
          neighbors2 = (length neighborsM2) + (length neighborsM2') + (length neighborsM2'') + (length m2Moves)

          removes1 = (length removeNeighborsM1) + (length removeNeighborsM1') -- + (length removeNeighborsM1'')
          removes2 = (length removeNeighborsM2) + (length removeNeighborsM2') -- + (length removeNeighborsM2'')
          m1 = neighbors1 - removes2
          m2 = neighbors2 - removes1


genMoves' :: State -> [Move]
genMoves' st = [Move {jumpTo = j, remove = r}
              | j <- jumps,
              r <- filter (\t -> t /= j && notElem t (neighbors'' allTiles j)) allTiles
              ]
     where allTiles = tiles st
           jumps = filter (\t -> isNeighbor t playerPos && t /= opponentPos) allTiles
           (playerPos, opponentPos) = if pl1Turn st then (pl1 st, pl2 st)
                                                    else (pl2 st, pl1 st)

-- Get the positions the player can jump to.
jumpToPositions :: [Move] -> [(Int, Int)]
jumpToPositions moves = nubBy (\x y -> x == y) [ jumpTo x | x <- moves]

removePositions :: [Move] -> [(Int, Int)]
removePositions moves = nubBy (\x y -> x == y) [ remove x | x <- moves]

-- Calculate the number of neighbors for any tile.
neighbors' :: (Ord a, Num a) => [(a, a)] -> (a, a) -> Int
neighbors' alltiles position = countNeighbors
   where countNeighbors = length $ moves
         moves = filter (\t -> isNeighbor t position) alltiles

-- Get the tiles that are neighbors of the tile in 'position'.
neighbors'' :: (Ord a, Num a) => [(a, a)] -> (a, a) -> [(a, a)]
neighbors'' alltiles position = countNeighbors
    where countNeighbors = moves
          moves = filter (\t -> isNeighbor t position) alltiles

isNeighbor :: (Ord a, Num a) => (a, a) -> (a, a) -> Bool
isNeighbor (x1, y1) (x2, y2) = max (abs $ x2 - x1) (abs $ y2 - y1) == 1
