module AStarSearch where

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector --Helps in simplified calling of functions
import BoardProps
import Data.Maybe (mapMaybe,fromMaybe)
import IndexConvert 


--What possible moves can take place from a given state
data Move = U | D | L | R

--Any state of the game is defined by 
--1) The configuration of the baord
--2) The key value, which is the manhattan disance from the solved state
--3) The dimenson/size of the board
--4) The position of the empty tile
--5) The number of moves that has been commpleted yet
--6) The previous state that the board was in
data State = State { numVec		:: Vector Int
					,distKey	:: Int
					,dimension	:: Int
					,emptyTilePos 	:: (Int, Int)
					,moves		:: Int
					,previous	:: Maybe State} deriving (Show,Eq,Ord)

--A function to create a state from an array of tiles.
createState :: [Int] -> State
createState xs = State nVec d n eTP 0 Nothing
	where
		nVec = Vector.fromList xs
		n = dimFinder nVec
		d = totalManDist nVec n
		eTP = one2two n (fromMaybe (error "Blank Tile missing") (Vector.elemIndex 0 nVec))

--A function that returns the new State after a move m has been performed on a state
move :: State -> Move -> State
move s m = s { numVec = nVec, distKey = totalManDist nVec n, dimension = dimension s, emptyTilePos=one2two n k, moves= moves s + 1, previous = Just s }
	where
		n= dimension s
		k= case m of
			U -> two2one n (fst (emptyTilePos s) - 1) (snd (emptyTilePos s))
			D -> two2one n (fst (emptyTilePos s) + 1) (snd (emptyTilePos s))
			R -> two2one n (fst (emptyTilePos s)) (snd (emptyTilePos s) + 1)
			L -> two2one n (fst (emptyTilePos s)) (snd (emptyTilePos s) - 1)
		nVec = numVec s // [(two2one n (fst (emptyTilePos s)) (snd (emptyTilePos s)), (numVec s) ! k), (k, 0) ]

--A function that returns the state, if a specified move m can be made. otherwise returns nothing
neighbour :: State -> Move -> Maybe State
neighbour s m = case m of
	U -> if i<=0 then Nothing else Just (move s m)
	D -> if i>=n-1 then Nothing else Just (move s m)
	L -> if j<=0 then Nothing else Just (move s m)
	R -> if j>=n-1 then Nothing else Just (move s m)
	where
		(i,j)= emptyTilePos s
		n = dimension s

neighbourBind :: State -> Move -> State
neighbourBind s m = case m of
	U -> if i<=0 then s else move s m
	D -> if i>=n-1 then s else move s m
	L -> if j<=0 then s else move s m
	R -> if j>=n-1 then s else move s m
	where
		(i,j)= emptyTilePos s
		n = dimension s

--A function that returns the possible states that we can go to from a particular state
neighbours :: State -> [State]
neighbours s = mapMaybe (neighbour s) [U,D,L,R]