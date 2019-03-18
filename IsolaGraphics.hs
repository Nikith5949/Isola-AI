module IsolaGraphics where

import Data.Char (ord)
import Data.List
import Data.Function
import Control.Concurrent
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
import Control.Monad
import IsolaShared

spacing = 0.35

display :: IORef State -> IORef (GLfloat, GLfloat) -> IORef (Move, Int, Int) -> MVar Move -> DisplayCallback
display board angles animate nextMove = do 
    clear [ColorBuffer, DepthBuffer] 
    clear [ColorBuffer]
    loadIdentity
    
    (currMove, jumpFrame, removeFrame) <- get animate
    if currMove == nullMove 
    then do m <- tryTakeMVar nextMove
            case m of Nothing -> return ()                            
                      Just mv -> if mv == nullMove then do board $= startState
                                                           animate $= (mv, 0, 0)
                                                   else animate $= (mv, 0, 0)
    else if jumpFrame == 10 && removeFrame < 10
         then animate $= (currMove, jumpFrame, removeFrame + 1)
         else if jumpFrame == 10 && removeFrame == 10              
              then do animate $= (nullMove, 0, 0)
                      board $~! \x -> execMove x currMove
              else animate $= (currMove, jumpFrame + 1, 0)
    (currMove2, jumpFrame2, removeFrame2) <- get animate
    b <- get board
    let (p1Trans, p2Trans) = getJumpPos b currMove2 jumpFrame
    
    translate $ Vector3 (0::GLfloat) 0 (-5)
    preservingMatrix $ do
        (alpha, beta) <- get angles
        rotate alpha $ Vector3 1.0 0.0 0.0
        rotate beta $ Vector3 0.0 1.0 0.0
        scale 0.7 0.7 (0.7::GLfloat)
        let factor = 1.0 - (fromIntegral removeFrame)/10.0
        forM_ (points b (remove currMove2)) $ \(x, y, z, r) -> preservingMatrix $ do
            color $ Color3 (0.7::GLfloat) 1.0 0.6
            translate $ Vector3 x y z
            if r then cuboid (0.1*factor) (0.05*factor) (0.1*factor)
                 else cuboid 0.1 0.05 0.1
            color $ Color3 (0::GLfloat) 0 0 
            cuboidFrame 0.1 0.05 0.1         
        preservingMatrix $ do
            color $ Color3 (0.8::GLfloat) 0.2 0.2
            translate $ Vector3 (spacing*(fromIntegral $ fst (pl1 b) - 4)::GLfloat) 0.2 (spacing*(4.0 - (fromIntegral $ snd $ pl1 b)))
            translate p1Trans
            cuboid 0.07 0.15 0.01
            color $ Color3 (0::GLfloat) 0 0 
            cuboidFrame 0.07 0.15 0.01 
        preservingMatrix $ do
            color $ Color3 (0.2::GLfloat) 0.2 0.8
            translate $ Vector3 (spacing*(fromIntegral $ fst (pl2 b) - 4)::GLfloat) 0.2 (spacing*(4.0 - (fromIntegral $ snd $ pl2 b)))
            translate p2Trans
            cuboid 0.07 0.15 0.01
            color $ Color3 (0::GLfloat) 0 0 
            cuboidFrame 0.07 0.15 0.01             
    swapBuffers

getJumpPos :: State -> Move -> Int -> (Vector3 GLfloat, Vector3 GLfloat) 
getJumpPos state move frame
    | move == nullMove = (Vector3 (0::GLfloat) 0 0, Vector3 (0::GLfloat) 0 0)
    | pl1Turn state    = (jPos $ pl1 state, Vector3 (0::GLfloat) 0 0) 
    | otherwise        = (Vector3 (0::GLfloat) 0 0, jPos $ pl2 state) 
    where jPos (h, v) = Vector3 ((fromIntegral ((fst (jumpTo move) - h) * frame)/10 * spacing)::GLfloat)  
                                (fromIntegral (25 - (5 - frame)^2)/100.0)
                                (fromIntegral ((v - (snd (jumpTo move))) * frame)/10 * spacing)
                                
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
 
cuboid :: GLfloat -> GLfloat -> GLfloat -> IO ()
cuboid u v w = renderPrimitive Quads $ mapM_ vertex3f
    [ ( w, v, w), ( w, v,-w), ( u,-v,-u), ( u,-v, u),
      ( w, v, w), ( w, v,-w), (-w, v,-w), (-w, v, w),
      ( w, v, w), ( u,-v, u), (-u,-v, u), (-w, v, w),
      (-w, v, w), (-w, v,-w), (-u,-v,-u), (-u,-v, u),
      ( u,-v, u), ( u,-v,-u), (-u,-v,-u), (-u,-v, u),
      ( w, v,-w), ( u,-v,-u), (-u,-v,-u), (-w, v,-w) ]

cuboidFrame :: GLfloat -> GLfloat -> GLfloat -> IO ()
cuboidFrame u v w = renderPrimitive Lines $ mapM_ vertex3f
    [ ( u,-v, u), ( w, v, w),  ( w, v, w), (-w, v, w),
      (-w, v, w), (-u,-v, u),  (-u,-v, u), ( u,-v, u),
      ( u,-v, u), ( u,-v,-u),  ( w, v, w), ( w, v,-w),
      (-w, v, w), (-w, v,-w),  (-u,-v, u), (-u,-v,-u),
      ( u,-v,-u), ( w, v,-w),  ( w, v,-w), (-w, v,-w),
      (-w, v,-w), (-u,-v,-u),  (-u,-v,-u), ( u,-v,-u) ]

points :: State -> (Int, Int) -> [(GLfloat, GLfloat, GLfloat, Bool)]
points b remTile = [(0.35*(fromIntegral h - 4.0), 0, spacing*(4.0 - fromIntegral v), (h, v) == remTile) | (h, v) <- tiles b]

idle :: IdleCallback
idle = do postRedisplay Nothing

reshape :: ReshapeCallback
reshape size = do 
    viewport $= (Position 0 0, size)
 
keyboardMouse :: IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse a key Down _ _ = case key of
    (SpecialKey KeyUp)    -> a $~! \(x,y) -> (x + 1.0, y)
    (SpecialKey KeyDown)  -> a $~! \(x,y) -> (x - 1.0, y)
    (SpecialKey KeyLeft)  -> a $~! \(x,y) -> (x, y + 1.0)
    (SpecialKey KeyRight) -> a $~! \(x,y) -> (x, y - 1.0)
    _ -> return ()
keyboardMouse _ _ _ _ _ = return ()

