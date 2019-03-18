module IsolaShared where

import Data.Char (ord)
import Data.Ord
import Data.List
import Data.Function
import System.IO

secPerMove = 3.0

mcrsecPerMove :: Int
mcrsecPerMove = round $ 1e6*secPerMove 

mcrsecTolerance :: Int
mcrsecTolerance = 200000

data Move = Move {jumpTo :: (Int, Int), remove :: (Int, Int)} deriving Eq

nullMove = Move {jumpTo = (0, 0), remove = (0, 0)} 

instance Show Move where 
    show (Move jumpTo remove) = "Jump to " ++ coords jumpTo ++ ", remove " ++ coords remove
        where coords (x, y) = ["ABCDEFG" !! (x - 1)] ++ show y

data State = State {pl1 :: (Int, Int), pl2 :: (Int, Int), tiles :: [(Int, Int)], pl1Turn :: Bool}

startState = State {pl1 = (4, 1), pl2 = (4, 7), pl1Turn = True,
                      tiles = [(x, y) | x <- [1..7], y <- [1..7], notElem (x, y) [(4, 1), (4, 7)]]}

instance Show State where
    show (State p1 p2 t _) = foldl (++) "" [hLine ++ drawRow y | y <- [7,6..1]] ++ hLine ++
                              "    A   B   C   D   E   F   G\n"
                              where hLine = "\n  +---+---+---+---+---+---+---+\n"
                                    drawRow row = show row ++ " |" ++ 
                                                  foldl (++) "" [" " ++ piece (x, row) ++ " |" | x <- [1..7]]  
                                    piece pos
                                        | pos == p1  = "1"
                                        | pos == p2  = "2"
                                        | elem pos t = "#"
                                        | otherwise  = " "
 
genMoves :: State -> [Move]
genMoves st = [Move {jumpTo = j, remove = r} | j <- jumps, r <- filter (\t -> t /= j && t /= opponentPos) allTiles]
           where allTiles = tiles st
                 jumps = filter (\t -> isNeighbor t playerPos && t /= opponentPos) allTiles
                 isNeighbor (x1, y1) (x2, y2) = max (abs $ x2 - x1) (abs $ y2 - y1) == 1
                 (playerPos, opponentPos) = if pl1Turn st then (pl1 st, pl2 st)
                                                          else (pl2 st, pl1 st)

execMove :: State -> Move -> State          
execMove st move
    | pl1Turn st = st {pl1 = jumpTo move, tiles = delete (remove move) (tiles st), pl1Turn = False}
    | otherwise  = st {pl2 = jumpTo move, tiles = delete (remove move) (tiles st), pl1Turn = True}
