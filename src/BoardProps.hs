module BoardProps where

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector --Helps in simplified calling of functions
import IndexConvert



--A function to find the board size from a given vector
dimFinder :: Vector Int-> Int
dimFinder nVec = round ( sqrt ( fromIntegral ( Vector.length nVec ) ) )

--A function that computes the manhattan distance of tile at i, j
distFinder :: Int -> Int -> Int -> Int  -> Int
distFinder v n i j = if v == 0 then 0 else yDist + xDist
  where
    yDist = abs (i - ((v-1) `div` n))
    xDist = abs (j - ((v-1) `mod` n))

--A function that computes the manhattan distance of the entire board. This is what is the key value of any state
totalManDist :: Vector Int -> Int -> Int
totalManDist nVec n  = sum [distFinder ( nVec ! (two2one n i j)) n i j | i<-[0..n-1], j<- [0..n-1]]