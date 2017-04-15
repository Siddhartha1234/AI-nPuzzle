module NPuzzle where

--We wil be using Vector LIbrary in our code. We will Also be using a Proirity Queue.
--loading only the specific things we need.
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector --Helps in simplified calling of functions
import Data.Maybe (mapMaybe,fromMaybe)
import qualified Data.PQueue.Prio.Min as PQ
import IndexConvert
import BoardProps
import BoardIO
import AStarSearch

type Que = PQ.MinPQueue Int State

--The main game AI. it takes in a state and returns a final state. We can go prev on the final state to get the sequence of steps to solve the input state
solve :: State -> State
solve s = go (PQ.fromList [(distKey s, s)])
	where
    go que = if distKey state == 0 
            then state 
            else go que2
            where
	        -- Retrieve the game state with the lowest priority and remove it from
	        -- the frontier.
	        ((_, state), que1) = PQ.deleteFindMin que

	        -- If the new board is the same as the previous board then
	        -- do not add it to the queue since it has already been explored.
	        neighbourStates = case previous state of
	        	Nothing -> neighbours state
	        	Just n-> filter (\x -> numVec x /= numVec n) (neighbours state)

	        keyStateMap = zip [moves q + distKey q | q <- neighbourStates] neighbourStates
	        que2 = foldr (uncurry PQ.insert) que1 keyStateMap