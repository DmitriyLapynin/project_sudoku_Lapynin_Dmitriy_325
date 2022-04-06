module Field where

import Type
import Data.Char

-- reading the game grid from a txt file
readField :: String -> Field
readField x = Field
    { gamegrid = makeGrid (lines x)
    , width = 9
    , height = 9
    , numbers = makeMatr (makeNum x) []
    , couple = (0, 0)
    }

-- make an array of numbers from a string
makeNum :: String -> [Int]
makeNum [] = []
makeNum (x:xs) = (ord(x) - ord('0')) : (makeNum xs)

-- make a matrix
makeMatr :: [Int] -> [Int]-> [[Int]]
makeMatr [] _ = [] 
makeMatr (x:xs) y = if (x == (ord('\n') - ord('0'))) then (y : (makeMatr xs []))
                    else (makeMatr xs (y ++ [x]))

-- make a field from a file
makeGrid :: [String] -> Grid
makeGrid x = map (fillLine) (map (makeNum) x)
           where fillLine = map (makeCell)


makeCell :: Int -> Cell
makeCell 0 = Cell {current = Empty}
makeCell _ = Cell {current = Const}

-- checking if there is a constant in the field
checkConst :: Grid -> Int -> Int -> Bool 
checkConst [] _ _ = False
checkConst (x : xs) a b = if (b /= 1) then (checkConst xs a (b - 1))
                          else checkConstX x a


checkConstX :: [Cell] -> Int -> Bool
checkConstX [] _ = False 
checkConstX (x : xs) a = if (a /= 1) then (checkConstX xs (a - 1))
                         else if (current x) == Const then True
                            else False

-- checking if there is a filled in the field
checkFilled :: Grid -> Int -> Int -> Bool 
checkFilled [] _ _ = False
checkFilled (x : xs) a b = if (b /= 1) then (checkFilled xs a (b - 1))
                          else checkFilledX x a


checkFilledX :: [Cell] -> Int -> Bool
checkFilledX [] _ = False 
checkFilledX (x : xs) a = if (a /= 1) then (checkFilledX xs (a - 1))
                         else if (current x) == Filled then True
                            else False

-- checking if there is a cell Ready in the field
checkReady :: Grid -> Int -> Int -> Bool 
checkReady [] _ _ = False
checkReady (x : xs) a b = if (b /= 1) then (checkReady xs a (b - 1))
                          else checkReadyX x a

checkReadyX :: [Cell] -> Int -> Bool
checkReadyX [] _ = False 
checkReadyX (x : xs) a = if (a /= 1) then (checkReadyX xs (a - 1))
                         else if (current x) == Ready then True
                            else False

-- element removal function
delNum :: Field -> Int -> Int -> Field
delNum f x y = f{numbers = addZero (numbers f) x y, gamegrid = makeEmpty (gamegrid f) x y}


-- instead of the deleted element, we substitute zero
addZero :: [[Int]] -> Int -> Int -> [[Int]]
addZero [] _ _ = []
addZero (x:xs) a b = if (b /= 1) then ([x] ++ (addZero xs a (b - 1)))
                          else ([addZeroX x a] ++ (addZero xs a (b - 1)))

addZeroX :: [Int] -> Int -> [Int]
addZeroX [] _ = []
addZeroX (x : xs) a = if (a /= 1) then (x : (addZeroX xs (a - 1)))
                          else (0 : (addZeroX xs (a - 1)))

-- making a cell with zero empty
makeEmpty :: Grid -> Int -> Int -> Grid
makeEmpty [] _ _ = []
makeEmpty (x:xs) a b = if (b /= 1) then ([x] ++ (makeEmpty xs a (b - 1)))
                        else ([makeEmptyX x a] ++ (makeEmpty xs a (b - 1)))

makeEmptyX :: [Cell] -> Int -> [Cell]
makeEmptyX [] _ = []
makeEmptyX (x : xs) a = if (a /= 1) then (x : (makeEmptyX xs (a - 1)))
                         else if (current x) == Filled then 
                             (Cell {current = Empty} : (makeEmptyX xs (a - 1))) 
                             else (x : (makeEmptyX xs (a - 1))) 

-- the function of adding an element
addNum :: Field -> Int -> Field
addNum f a = if (couple f) == (0, 0) then f 
            else f{numbers = addNumY (numbers f) (fst (couple f)) (snd (couple f)) a
                , gamegrid = makeFilled (gamegrid f) (fst (couple f)) (snd (couple f))
                , couple = (0, 0)}

-- instead of zero, we add an element
addNumY :: [[Int]] -> Int -> Int -> Int -> [[Int]]
addNumY [] _ _ _ = []
addNumY (x:xs) a b num = if (b /= 1) then ([x] ++ (addNumY xs a (b - 1) num))
                          else ([addNumX x a num] ++ (addNumY xs a (b - 1) num))

addNumX :: [Int] -> Int -> Int -> [Int]
addNumX [] _ _ = []
addNumX (x : xs) a num = if (a /= 1) then (x : (addNumX xs (a - 1) num))
                          else (num : (addNumX xs (a - 1) num))

-- making the cell with the element filled
makeFilled :: Grid -> Int -> Int -> Grid
makeFilled [] _ _ = []
makeFilled (x:xs) a b = if (b /= 1) then ([x] ++ (makeFilled xs a (b - 1)))
                        else ([makeFilledX x a] ++ (makeFilled xs a (b - 1)))

makeFilledX :: [Cell] -> Int -> [Cell]
makeFilledX [] _ = []
makeFilledX (x : xs) a = if (a /= 1) then (x : (makeFilledX xs (a - 1)))
                         else (Cell {current = Filled} : (makeFilledX xs (a - 1)))


-- paint the cell green
getReady :: Field -> Int -> Int -> Field
getReady f x y = f{gamegrid = makeReady (gamegrid f) x y, couple = func (fst (couple f)) x y}

func :: Int -> Int -> Int -> (Int, Int)
func a x y = if (a == x) then (0, 0)
             else (x, y)

-- go through the rows in the array to find the right one
makeReady :: Grid -> Int -> Int -> Grid
makeReady [] _ _ = []
makeReady (x:xs) a b = if (b /= 1) then ([x] ++ (makeReady xs a (b - 1))) 
                        else ([makeReadyX x a] ++ (makeReady xs a (b - 1)))


-- replace the element in the corresponding line
makeReadyX :: [Cell] -> Int -> [Cell]
makeReadyX [] _ = []
makeReadyX (x : xs) a = if (a /= 1) then (x : (makeReadyX xs (a - 1))) 
                          else if (current x) == Ready then 
                              (Cell {current = Empty} : (makeReadyX xs (a - 1)))
                              else if (current x) == Const then (x : (makeReadyX xs (a - 1)))
                                else if (current x) == Filled then (x : (makeReadyX xs (a - 1)))
                                else (Cell {current = Ready} : (makeReadyX xs (a - 1)))

