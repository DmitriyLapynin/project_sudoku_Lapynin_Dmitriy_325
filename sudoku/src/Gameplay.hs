module Gameplay where

import Graphics.Gloss.Interface.Pure.Game
import Type
import Field
import Graphic
import Data.List

-- update the field (does nothing)
fieldUpdate :: Float -> Field -> Field
fieldUpdate _ f = f

-- changing the state of the field
handleBEvent :: Event -> Field -> Field        
handleBEvent (EventKey (MouseButton LeftButton) Down _ mouse) f = if ((fst (couple f) == 0) 
                                                                  && (not (checkConst (gamegrid f) (mouseX x 1) (mouseY y 1)))                                                  
                                                                  && (not (checkFilled (gamegrid f) (mouseX x 1) (mouseY y 1))))
                                                                  || ((mouseX x 1) == fst (couple f) 
                                                                  && (mouseY y 1) == snd (couple f))
                                                                   then getReady f (mouseX x 1) (mouseY y 1)  
                                                                  else f
                                                                where
                                                                   x = floor(fst mouse)
                                                                   y = floor(snd mouse)
handleBEvent (EventKey (MouseButton RightButton) Down _ mouse) f = if (checkConst (gamegrid f) (mouseX x 1) (mouseY y 1)) 
                                                                   || (checkReady (gamegrid f) (mouseX x 1) (mouseY y 1)) then f
                                                                   else delNum f (mouseX x 1) (mouseY y 1)  
                                                                where
                                                                   x = floor(fst mouse)
                                                                   y = floor(snd mouse)                                                                                              
handleBEvent (EventKey (SpecialKey KeyF1) Down _ _) f = addNum f 1
handleBEvent (EventKey (SpecialKey KeyF2) Down _ _) f = addNum f 2
handleBEvent (EventKey (SpecialKey KeyF3) Down _ _) f = addNum f 3
handleBEvent (EventKey (SpecialKey KeyF4) Down _ _) f = addNum f 4
handleBEvent (EventKey (SpecialKey KeyF5) Down _ _) f = addNum f 5
handleBEvent (EventKey (SpecialKey KeyF6) Down _ _) f = addNum f 6
handleBEvent (EventKey (SpecialKey KeyF7) Down _ _) f = addNum f 7
handleBEvent (EventKey (SpecialKey KeyF8) Down _ _) f = addNum f 8
handleBEvent (EventKey (SpecialKey KeyF9) Down _ _) f = addNum f 9
handleBEvent _ f = f 


-- Get the coordinates of the cell under the mouse
mouseX :: Int -> Int -> Int 
mouseX x p = if (x <= -225 + p*50) then p else (mouseX x (p + 1))

mouseY :: Int -> Int -> Int 
mouseY y p = if (y >= 225 - p*50) then p else (mouseY y (p + 1))


playGame :: Field -> IO()
playGame f = play (display f) bgColor fps f drawGame handleBEvent fieldUpdate
        where
            display f = InWindow "Sudoku" ((screenWidth f), (screenHeight f)) (0, 0)
            bgColor = white
            fps = 60




run :: IO ()
run = do
    filecontent <- readFile "field.txt"
    let board = readField (filecontent)
    playGame board
   --filecontent <- readFile "field.txt"
   --print(matrOfSquare(makeMatr (makeNum filecontent) []))
      
       








