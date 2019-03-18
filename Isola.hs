module Main where

import Data.Char (ord)
import Data.Ord
import Data.List
import Data.Function
import Control.Concurrent
import System.IO
import System.IO.Unsafe
import System.Timeout
import Graphics.UI.GLUT
import Data.IORef
import IsolaShared
import IsolaGraphics

-- Import all players that you want to compete against each other as P1 ... PN
import qualified HorriblePlayer as P1 (getName, getMove)
import qualified WeakPlayer as P2 (getName, getMove)
import qualified SimplePlayer as P3 (getName, getMove)
import qualified WorstPlayer as P4 (getName, getMove)
import qualified Myplayer as P5 (getName, getMove)
import qualified NikithAnupkumar as P6 (getName, getMove)
import qualified FlavioAndrade2 as P7 (getName, getMove)
-- Here, uncomment and modify the kind of competition you want. 
-- isolaCompComp: Two comuter players competing in a single match
-- isolaHumanComp: Single match human against computer player
--isolaTournament: Double round-robin tournament with any number of players 
main = do nextMove <- newEmptyMVar
          threadID <- forkIO $ graphicsThread nextMove
          --isolaCompComp (P6.getName, P6.getMove) (P2.getName, P2.getMove) nextMove
          --isolaCompComp (P2.getName, P2.getMove) (P6.getName, P6.getMove) nextMove
          --isolaHumanComp (P5.getName, P5.getMove) nextMove
          --isolaTournament [(P1.getName, P1.getMove), (P2.getName, P2.getMove), (P3.getName, P3.getMove), (P4.getName, P4.getMove), (P6.getName, P6.getMove)] nextMove
          isolaTournament [(P5.getName, P5.getMove),(P6.getName, P6.getMove)] nextMove
          hFlush stdout
     
isolaTournament :: [(String , (State -> IO (Move, Int)))] -> MVar Move -> IO ()
isolaTournament players nextMove =
    do winnerList <- mapM playMatch matches
       let score = reverse $ sortBy (comparing snd) [(c, length $ filter (== c) winnerList) | c <- nub plList]
       prn "\n*** FINAL TOURNAMENT RESULT ***\n"
       mapM_ (\(pl, sc) -> prn $ (fst $ players !! pl) ++ ": " ++ show sc ++ " points") score   
       where playMatch (pl1, pl2) = do prn "\nNext match:"
                                       result <- isolaCompComp (players !! pl1) (players !! pl2) nextMove 
                                       threadDelay 3000000
                                       putMVar nextMove nullMove
                                       if result then return pl1 else return pl2
             matches = [(pl1, pl2) | pl1 <- plList, pl2 <- plList, pl1 /= pl2]
             plList = [0..(length players - 1)]
isolaCompComp :: (String , (State -> IO (Move, Int))) -> (String, (State -> IO (Move, Int))) -> MVar Move -> IO Bool                    
isolaCompComp (name1, move1) (name2, move2) nextMove = 
    do prn $ "Player 1 (Red) : " ++ name1
       prn $ "Player 2 (Blue): " ++ name2
       playTurn startState
       where playTurn st 
                 | gameOver st && not (pl1Turn st) = do prn $ "Player 1 (" ++ name1 ++ ") wins!\n"
                                                        return True
                 | gameOver st && pl1Turn st       = do prn $ "Player 2 (" ++ name2 ++ ") wins!\n"
                                                        return False
                 | pl1Turn st                      = do move <- compTurn st name1 move1
                                                        putMVar nextMove move
                                                        playTurn $ execMove st move 
                 | otherwise                       = do move <- compTurn st name2 move2
                                                        putMVar nextMove move
                                                        playTurn $ execMove st move

isolaHumanComp :: (String, (State -> IO (Move, Int))) -> MVar Move -> IO ()                    
isolaHumanComp (name2, move2) nextMove =
    do prn $ "Player 1 (Red) : Human"
       prn $ "Player 2 (Blue): " ++ name2
       playTurn startState
       where playTurn st 
                 | gameOver st && not (pl1Turn st) = prn $ show st ++ "Player 1 (Human) wins!\n"
                 | gameOver st && pl1Turn st       = prn $ show st ++ "Player 2 (" ++ name2 ++ ") wins!\n"
                 | pl1Turn st                      = do move <- humanTurn st
                                                        putMVar nextMove move
                                                        playTurn $ execMove st move 
                 | otherwise                       = do move <- compTurn st name2 move2
                                                        putMVar nextMove move
                                                        playTurn $ execMove st move
                      
humanTurn :: State -> IO Move       
humanTurn st = do putStr $ show st ++ "Your move: Jump to: "
                  hFlush stdout
                  (a:b:cs) <- getLine
                  putStr "Your move: Remove:  "
                  hFlush stdout
                  (d:e:fs) <- getLine
                  let move = Move {jumpTo = (mod (ord a) 16, mod (ord b) 16), remove = (mod (ord d) 16, mod (ord e) 16)}
                  if elem move (genMoves st) then return move
                                             else humanTurn st
     --}
     
compTurn :: State -> String -> (State -> IO (Move, Int)) -> IO Move     
compTurn st cName getMove = do (cMove, cLevel) <- getCompMove st getMove
                               prn $ cName ++ "'s move: " ++ show cMove ++ " (level " ++ show cLevel ++ ")"
                               return $ cMove

getCompMove :: State -> (State -> IO (Move, Int)) -> IO (Move, Int)
getCompMove state getMove = 
    do compMove <- timeout (mcrsecPerMove + mcrsecTolerance) (getMove state)
       return $ pickMove compMove
       where pickMove (Just mv) = if (fst mv) `elem` allMoves then mv else (head allMoves, -1)
             pickMove (Nothing) = (head allMoves, -1)
             allMoves = genMoves state

gameOver :: State -> Bool
gameOver st = null $ genMoves st
                     
graphicsThread :: MVar Move -> IO ()
graphicsThread nextMove = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    initialWindowSize  $= Size 1000 1000
    _window <- createWindow "Isola"
    reshapeCallback $= Just reshape
    depthFunc $= Just Less 
    angles <- newIORef (45.0, -15.0)
    board <- newIORef startState
    animate <- newIORef (nullMove, 0, 0)
    keyboardMouseCallback $= Just (keyboardMouse angles)
    idleCallback $= Just idle
    displayCallback $= display board angles animate nextMove
    matrixMode $= Projection
    loadIdentity
    let (near, far, right, top) =  (3, 7, 0.7, 0.7)
    frustum (-right) right (-top) top near far
    matrixMode $= Modelview 0
    mainLoop
 
prn :: String -> IO ()
prn text = do putStrLn text
              hFlush stdout
