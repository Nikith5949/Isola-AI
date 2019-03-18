module HorriblePlayer where

import IsolaShared

getName = "Horrible Player"

getMove :: State -> IO (Move, Int)
getMove state = return (head $ genMoves state, 0)


