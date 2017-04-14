module Main where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import NPuzzle
import IndexConvert
import BoardProps
import BoardIO
import AStarSearch

isSolvable :: [Int]->Int->Bool
isSolvable nums n = (blankTileRow + inverts) `mod` 2 ==0
	where
		blankTileRow = 1 + fromJust (0 `elemIndex` nums ) `div` n
		inverts= length [x | (x,i) <- zip nums [1..], (y,j) <- zip nums [1..], x/=0, y/=0, x>y, j>i ]

--A function to cretate a list of Int from the input string
fromString :: String -> [[Int]]
fromString s = (map . map) read ws
  where ws = map words (lines s)

main = do
  putStrLn "Enter the name of the file containing the puzzle specification: "
  txt <- readFile =<< getLine
  let game = fromString txt
      ([n], brd) = case game of
        [] -> error "Invalid puzzle file"
        x:xs -> (x, concat xs)
  let p = solve (createState brd)
  printer (boards p)