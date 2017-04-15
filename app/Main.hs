module Main where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import NPuzzle
import IndexConvert
import BoardProps
import BoardIO
import AStarSearch

import DisplayNPuzzle
import BindingNPuzzle
import Data.IORef
import Graphics.UI.GLUT

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector --Helps in simplified calling of functions

--A function to cretate a list of Int from the input string
fromString :: String -> [[Int]]
fromString s = (map . map) read ws
  where ws = map words (lines s)

main = do
  putStrLn "Enter the name of test file "
  inputTxt <- readFile =<< getLine
  let config = fromString inputTxt
  let ([n], numVec) = case config of
        [] -> error "Invalid puzzle file"
        x:xs -> (x, concat xs)

  (_progName, _args) <- getArgsAndInitialize
  initialWindowSize $= Size 600 600
  _window <- createWindow "NPuzzle"
  
  
  position <- newIORef (Position 0 0)
  size  <- newIORef n
  list  <- newIORef (createState numVec)
  flag <- newIORef False
  gameOver <- newIORef False
  _n <- newIORef n
  
  displayCallback $= displayPuzzle position size list flag gameOver
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse list _n)
  idleCallback $= Just idle
  mainLoop