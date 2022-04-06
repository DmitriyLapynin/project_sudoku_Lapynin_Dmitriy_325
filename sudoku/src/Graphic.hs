-- Drawing the playing field
module Graphic where

import Graphics.Gloss.Interface.Pure.Game
import Type
--import Field
import Data.List

-- cell size in pixels
cellSize :: Int
cellSize = 50

-- indent in pixels
indent :: Int
indent = 10


-- recalculate the size in cells
getSize :: Int -> Int
getSize x = x * cellSize

-- screen width in pixels
screenWidth :: Field -> Int
screenWidth f = getSize (width f) + 2*indent

-- screen height in pixels
screenHeight :: Field -> Int
screenHeight f = getSize (height f) + 2*indent

-- draw a game
drawGame :: Field -> Picture
drawGame f 
      = Translate (x) (y) (Pictures [drawGrid f, drawConst f, drawNums f, drawCell f, drawWin f])
           where
           x = - fromIntegral (screenWidth f)  / 2
           y = - fromIntegral (screenHeight f) / 2

-- draw a grid
drawGrid :: Field -> Picture
drawGrid f = Pictures (hl ++ vl ++ hlColor ++ vlColor)
           where
           hlColor = map (\y -> Color red $ Line [(i,y), (i+dx,y)]) 
                                                [l*c + j | l <- [0..h]
                                                , l == 0 || l == 3 || l == 6 || l == 9] 
           hl = map (\y -> Line [(i,y), (i+dx,y)]) [l*c + j | l <- [0..h]]
           vlColor = map (\x -> Color red $ Line [(x,j), (x,dy+j)]) 
                                                [l*c + i | l <- [0..w]
                                                , l == 0 || l == 3 || l == 6 || l == 9] 
           vl = map (\x -> Line [(x,j), (x,dy+j)]) [l*c + i | l <- [0..w]]
           i = fromIntegral indent
           j = fromIntegral indent
           dx = fromIntegral (getSize(width f))
           dy = fromIntegral (getSize(height f))
           w = fromIntegral (width f)
           h = fromIntegral (height f)
           c = fromIntegral cellSize

-- draw text by coordinates
drawNum :: [Float] -> [Float] -> [Int] -> Picture
drawNum x y l = Pictures (zipWith3 (Translate) x y (map (scale compr compr . Text . show) l))
              where compr = 0.3

drawNums :: Field -> Picture
drawNums f = Pictures [hl]
           where
           hl = drawNum (makeX num) (makeY num 0) (del_Zero num)
           num = numbers f   

-- get the coordinates of the initial numbers
makeX :: [[Int]] -> [Float]
makeX [] = []
makeX (x:xs) = (changeX x 0) ++ (makeX xs)

changeX :: [Int] -> Float -> [Float]
changeX [] _ = []
changeX x p = if ((head x) /= 0) then (24.0 + a * p) : (changeX (tail x) (p + 1))
                else (changeX (tail x) (p + 1))
              where 
                a = fromIntegral cellSize

makeY :: [[Int]] -> Float -> [Float]
makeY [] _ = []
makeY (x:xs) y = (changeY x y) ++ (makeY xs (y + 1.0))

changeY :: [Int] -> Float -> [Float]
changeY [] _ = []
changeY x p = if ((head x) /= 0) then (420.0 - a * p) : (changeY (tail x) p)
                else (changeY (tail x) p)
              where 
                a = fromIntegral cellSize


-- removing null elements from the array. They do not need to be printed in the initial state
del_Zero :: [[Int]] -> [Int]
del_Zero [] = []
del_Zero (x : xs) = (del_Zero_row x) ++ (del_Zero xs)

del_Zero_row :: [Int] -> [Int]
del_Zero_row [] = []
del_Zero_row (x : xs) = if (x == 0) then (del_Zero_row xs)
                        else (x : del_Zero_row xs)


-- paint over the cell
drawMark :: Float -> Float -> State -> Picture
drawMark x y m 
  | m == Ready = Translate (x) (y) (Color (green) (Polygon [(1, 2), (1,  a+1), (a, a+1), (a, 2)]))
  | m == Empty = Translate (x) (y) (Color (white) (Polygon [(1, 2), (1,  a+1), (a, a+1), (a, 2)]))
  | otherwise = Blank
    where a = fromIntegral (cellSize - 2)

drawMarkGrey :: Float -> Float -> State -> Picture
drawMarkGrey x y m 
  | m == Const = Translate (x) (y) (Color (greyN 0.92) (Polygon [(1, 2), (1,  a+1), (a, a+1), (a, 2)]))
  | otherwise = Blank
    where a = fromIntegral (cellSize - 2)

-- highlight constant numbers
drawConst :: Field -> Picture
drawConst f = pictures drawCells
           where
           drawCells = foldMap draw1 (zip [0..] (gamegrid f))
           draw1 (j, linecell) = map draw2 (zip [0..] linecell)
             where
             draw2 (i, cell) = drawMarkGrey (fromIntegral (indent + (getSize i))) 
                                            (fromIntegral (indent + (getSize ((height f) - j - 1)))) 
                                            (current cell)

-- painting over the playing field             
drawCell :: Field -> Picture
drawCell f = pictures drawCells
           where
           drawCells = foldMap draw1 (zip [0..] (gamegrid f))
           draw1 (j, linecell) = map draw2 (zip [0..] linecell)
             where
             draw2 (i, cell) = drawMark (fromIntegral (indent + (getSize i))) 
                                        (fromIntegral (indent + (getSize ((height f) - j - 1)))) 
                                        (current cell)

-- draw text with victory
drawWin :: Field -> Picture
drawWin f 
  | ((checkStr (numbers f)) 
    && (checkStr (transpose (numbers f))) 
    && (checkStr (matrOfSquare (numbers f)))) 
    = Pictures (Translate (x) (y) (Color (green) (Polygon [(-100, 40), (-100,  120), (260, 120), (260, 40)])) 
    : [Color (black) (Translate 130 200 (scale compr compr (Text "You Win!")))])
  | otherwise = Blank
            where
              x = fromIntegral (screenWidth f) / 2 - 80
              y = fromIntegral (screenHeight f) / 2 - 100
              compr = 0.4


-- check the rows for the same element
checkStr :: [[Int]] -> Bool
checkStr [] = True 
checkStr (x : xs) = if (checkStrX x) then (checkStr xs)
                    else False

checkStrX :: [Int] -> Bool 
checkStrX [] = True
checkStrX (x : xs) = if (x == 0) || (elem x xs) then False
                     else (checkStrX xs)

-- creating a matrix where the rows are 3x3 squares of the original matrix
matrOfSquare :: [[Int]] -> [[Int]]
matrOfSquare [] = []
matrOfSquare (x1 : x2 : x3 : xs) 
    = ((take 3 x1) ++ (take 3 x2) ++ (take 3 x3)) 
    : ((take 3 (reverse(take 6 x1))) ++ (take 3 (reverse(take 6 x2))) ++ (take 3 (reverse(take 6 x3))))
    : ((take 3 (reverse x1)) ++ (take 3 (reverse x2)) ++ (take 3 (reverse x3))) 
    : (matrOfSquare xs)
matrOfSquare _ = [] 

