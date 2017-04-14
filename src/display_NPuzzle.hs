module DisplayNPuzzle where

import Graphics.UI.GLUI
import GraphicsUtils

displayPuzzle:: IORef Position -> IORef [Int] -> IORef Bool -> IORef Bool -> DisplayCallback
displayPuzzle pos list checkClick gameEnded = do
	clear [ColorBuffer]
	loadIdentity
	_check <- readIORef checkClick
	(Position x1 y1) <- readIORef pos
	_matrix <- readIORef list

	forM_  $ \(x,y,z) ->
	

	flush









