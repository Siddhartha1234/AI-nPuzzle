module Main where

--import Klotski
--import Npuzzle
import DisplayNPuzzle
import DisplayKlotski


main :: IO ()
main = do
	putStrLn "Enter name of the puzzle :\n1. n-Puzzle\n2. Klotski" 
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "N - Puzzle"
	reshapeCallback $= Just reshape
	keyboardMouseCallback $= Just keyboardMouse



