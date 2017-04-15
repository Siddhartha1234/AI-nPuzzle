module DisplayNPuzzle where

import Graphics.UI.GLUT hiding (Front)
import Graphics.Rendering.FTGL 
import GraphicsUtils

import Data.IORef
import Control.Monad
import IndexConvert

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector --Helps in simplified calling of functions
import BoardProps
import BoardIO
import AStarSearch

displayPuzzle:: IORef Position -> IORef Int -> IORef (State) -> IORef Bool -> IORef Bool -> DisplayCallback
displayPuzzle pos size list checkClick gameEnded = do
	clear [ColorBuffer]
	loadIdentity
	_check <- readIORef checkClick
	(Position x1 y1) <- readIORef pos
	__size <- readIORef size
	_matr <- readIORef list
	forM_ (points __size __size __size) $ \(x,y,z) ->
		preservingMatrix $ do
			let _size = (fromIntegral __size)
			translate $ Vector3 x y 0
			color $ Color3 (0.75) (0.75) (0.75::GLfloat)
			cube (1.0/( _size))
			color $ Color3 (0) (0) (0::GLfloat)
			translate $ Vector3 (-x) (-y) 0
			draw_line (x +  (1.0/_size),-1) (x +  (1.0/_size), 1) 
			draw_line (-1, (1.0/_size) + y) (1,  (1.0/_size) + y)
			drawDigit (int_to_string ((numVec _matr) ! (two2one (dimFinder (numVec _matr)) (fst (get_index_from_point (x, y) _size) - 1) (snd (get_index_from_point (x, y) _size) -1 )))) (length_string (int_to_string ((numVec _matr) ! (two2one (dimFinder (numVec _matr)) (fst (get_index_from_point (x, y) _size) -1) (snd (get_index_from_point (x, y) _size) -1))))) x  y _size  
			translate $ Vector3 x y 0
	flush









