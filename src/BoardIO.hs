module BoardIO where

import AStarSearch
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector --Helps in simplified calling of functions

--A function that returns the list of states that the puzzle goes into as we solve it
boards :: State -> [[Int]]
boards s = map Vector.toList (reverse (nVec s))
  where
    nVec s = case previous s of
      Nothing -> [numVec s]
      Just r  -> numVec s : nVec r

--A function to print the solution
printer :: [[Int]]->IO()
printer [] =return ()
printer (x:xs) = do printElements x
                    putStrLn ""
                    printer xs

--A function to print the elemnts of an array
printElements :: [Int] -> IO()
printElements [] = return ()
printElements (x:xs) = do putStr (show x)
                          putStr " "
                          printElements xs