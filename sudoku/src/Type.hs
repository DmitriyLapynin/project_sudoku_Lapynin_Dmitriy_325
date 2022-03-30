module Type where

data State = Filled | Ready | Const | Empty deriving (Show, Eq) -- cell status: filled, constant, ready or empty

data Cell = Cell {current :: State} deriving Show 

type Grid = [[Cell]] -- the playing field is a matrix of cells
 
-- the playing field
data Field = Field 
    { gamegrid :: Grid -- game grid
    , width :: Int -- the width of the playing field in cells
    , height :: Int -- the height of the playing field in cells
    , numbers :: [[Int]] -- numbers
    , couple :: (Int, Int) -- the coordinate of the ready cell
    }