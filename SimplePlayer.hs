module SimplePlayer where

import Data.Char (ord)
import Data.Ord
import Data.List
import Data.Function
import System.IO
import IsolaShared

getName = "Simple Player"

getMove :: State -> IO (Move, Int)
getMove state = return (fst $ head $ sortedMoves state, 1)

sortedMoves :: State -> [(Move, Double)]
sortedMoves st = if pl1Turn st then reverse sortedList else sortedList
    where sortedList = sortBy (comparing snd) moveList 
          moveList = [(m, score $ execMove st m) | m <- genMoves st]
       
score :: State -> Double                                                          
score st
    | m1 == 0 && m2 == 0 && pl1Turn st = -1000.0
    | m1 == 0 && m2 == 0               =  1000.0
    | m1 == 0                          = -1000.0
    | m2 == 0                          =  1000.0
    | otherwise                        = fromIntegral (m1 - m2)
    where m1 = length $ genMoves st {pl1Turn = True}
          m2 = length $ genMoves st {pl1Turn = False}
