module BindingNPuzzle where

import Graphics.UI.GLUT hiding (R)
import Data.IORef
import AStarSearch
import NPuzzle

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector --Helps in simplified calling of functions

import Data.Maybe
import Data.List

import BoardIO

isSolvable :: [Int]->Int->Bool
isSolvable nums n = (blankTileRow + switches) `mod` 2 ==0
	where
		blankTileRow = 1 + fromJust (elemIndex 0 nums ) `div` n
		switches = length [x | (x,i) <- zip nums [1..], (y,j) <- zip nums [1..], x/=0, y/=0, x>y, j>i ]

reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)
 
keyboardMouse :: IORef State -> IORef Int -> KeyboardMouseCallback
keyboardMouse  state _n key Down _ _ = case key of
  (SpecialKey KeyLeft ) ->  do
  								_state <- readIORef state
  								let a = neighbourBind _state L
  								state $~! \x ->  a
  (SpecialKey KeyRight) -> do
  								_state <- readIORef state
  								let a = neighbourBind _state R
  								state $~! \x ->  a
  (SpecialKey KeyUp   ) -> do
  								_state <- readIORef state
  								let a = neighbourBind _state U
  								state $~! \x ->  a
  (SpecialKey KeyDown ) -> do
  								_state <- readIORef state
  								let a = neighbourBind _state D
  								state $~! \x ->  a
  (SpecialKey KeyCtrlL)    -> do
  								_state <- readIORef state
  								n <- readIORef _n
  								if (isSolvable (Vector.toList (numVec _state)) n) then
  									printer (boards (solve (createState (Vector.toList (numVec _state)))))
  								else putStrLn "Unsolvable"

  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()

idle :: IdleCallback
idle = do
	postRedisplay Nothing