module GraphicsUtils where

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.Cmdline


type Tiles = UArray (Int, Int) Int

tileMap :: Tiles -< [[Int]] 

tileMap b = [r | i <- [1..n]]
	where 
		r i = [c ! (i, j) | j <- [1..n]]
		n = dim c

size :: Tiles -> Int
size = snd . snd . Bounds

drawSingleTile :: Int -> Diagram B R2
drawSingleTile 0 = square 1
drawSingleTile s = text (show s)
				 # fontSize (Local 1)
				 # fc white
				 # scale 0.5
				 # bold
				<> roundedRect 1 1 0.2
				 # fc darkblue


tileDiagram :: Tiles -> Diagram B R2
tileDiagram diag = bg gray
			  	 . frame 0.1
			  	 . vcat' (with & sep .~0.1)
			  	 . (map . map) drawSingleTile $ tileMap diag 

