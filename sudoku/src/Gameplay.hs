module Gameplay where

import Graphics.Gloss.Interface.Pure.Game
import Type
import Field
import Graphic
import Check

fileGame :: FilePath 
fileGame = "field.txt"

-- update the field (does nothing)
fieldUpdate :: Float -> Field -> Field
fieldUpdate _ f = f

-- changing the state of the field
handleBEvent :: Event -> Field -> Field        
handleBEvent (EventKey (MouseButton LeftButton) Down _ mouse) f 
                                    = if ((fst (couple f) == 0) 
                                    && (not (checkConst (gamegrid f) (mouseX f x 1) (mouseY f y 1)))                                                  
                                    && (not (checkFilled (gamegrid f) (mouseX f x 1) (mouseY f y 1))))
                                    || ((mouseX f x 1) == fst (couple f) 
                                    && (mouseY f y 1) == snd (couple f))
                                    then getReady f (mouseX f x 1) (mouseY f y 1)  
                                       else f
                                          where
                                             x = fst mouse
                                             y = snd mouse
handleBEvent (EventKey (MouseButton RightButton) Down _ mouse) f 
                                    = if (checkConst (gamegrid f) (mouseX f x 1) (mouseY f y 1)) 
                                    || (checkReady (gamegrid f) (mouseX f x 1) (mouseY f y 1)) then f
                                       else delNum f (mouseX f x 1) (mouseY f y 1)  
                                          where
                                          x = fst mouse
                                          y = snd mouse                                                                                             
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
mouseX :: Field -> Float -> Int -> Int 
mouseX f x p = if (x <= -a + b) then p else (mouseX f x (p + 1))
               where 
                  a = fromIntegral (screenWidth f) / 2
                  b = fromIntegral (getSize p)

mouseY :: Field -> Float -> Int -> Int 
mouseY f y p = if (y >= a - b) then p else (mouseY f y (p + 1))
             where 
                  a = fromIntegral (screenWidth f) / 2
                  b = fromIntegral (getSize p)

playGame :: Field -> IO()
playGame f = play (display f) bgColor fps f drawGame handleBEvent fieldUpdate
        where
          display x = InWindow "Sudoku" ((screenWidth x), (screenHeight x)) (0, 0)
          bgColor = white
          fps = 60




run :: IO ()
run = do
   file <- readFile fileGame
   if (lines file) /= [] && length(lines file) == 9 && checkConf (lines file) then do
      let board = readField file
      playGame board
   else putStrLn ("Error. Wrong format of file " ++ fileGame) 
      
       








