module Main where

--We wil be using Vector LIbrary in our code. We will Also be using a Proirity Queue.
--loading only the specific things we need.
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector --Helps in simplified calling of functions
import Data.Maybe (mapMaybe,fromMaybe)
import qualified Data.PQueue.Prio.Min as PQ


--We represent the board with a 1D array of n*n elements, where n is the size of the puzzle.
type Board= Vector Int

type Frontier = PQ.MinPQueue Int State

--defining some data types needed.

--What possible moves can take place from a given state
data Move = U | D | L | R

--Any state of the game is defined by 
--1) The configuration of the baord
--2) The key value, which is the manhattan disance from the solved state
--3) The dimenson/size of the board
--4) The position of the empty tile
--5) The number of moves that has been commpleted yet
--6) The previous state that the board was in
data State = State { board		:: Board
					,distKey	:: Int
					,dimension	:: Int
					,emptyTile 	:: Int
					,moves		:: Int
					,previous	:: Maybe State} deriving (Show,Eq,Ord)

--A function that converts the indices of a 2D array to the corresponding indice in a 1D array
two2one :: Int -> Int -> Int -> Int
two2one n row coloumn = n * row + coloumn

--A function that converts the indices of a 1D array to the corresponding indices in a 2D array
one2two :: Int -> Int -> (Int, Int)
one2two n i = (i `div` n, i `mod` n)

boardSize :: Board-> Int
boardSize b = round ( sqrt ( fromIntegral ( Vector.length b ) ) )

--A function that computes the manhattan distance of tile at i, j
manhattan :: Int -> Int -> Int -> Int  -> Int
manhattan v n i j = if v == 0 then 0 else rowDist + colDist
  where
    rowDist = abs (i - ((v-1) `div` n))
    colDist = abs (j - ((v-1) `mod` n))

--A function that computes the manhattan distance of the entire board. This is what is the key value of any state
totalManDist :: Board -> Int -> Int
totalManDist b n  = sum [manhattan ( b ! (two2one n i j)) n i j | i<-[0..n-1], j<- [0..n-1]]

--A function to create a state from an array of tiles.
createState :: [Int] -> State
createState xs = State b d n eTP 0 Nothing
	where
		b = Vector.fromList xs
		n = boardSize b
		d = totalManDist b n
		eTP = fromMaybe (error "Invalid board - Blank Tile missing") (Vector.elemIndex 0 b)

--A function that returns the new State after a move m has been performed on a state
move :: State -> Move -> State
move s m = s { board = b, distKey = totalManDist b n, dimension = dimension s, emptyTile=k, moves= moves s + 1, previous = Just s }
	where
		n= dimension s
		k= case m of
			U -> emptyTile s - n
			D -> emptyTile s+ n
			R -> emptyTile s+ 1
			L -> emptyTile s-1
		b = bCurr // [(emptyTile s, bCurr ! k), (k, 0) ]
		bCurr = board s

--A function that returns the state, if a specified move m can be made. otherwise returns nothing
neighbour :: State -> Move -> Maybe State
neighbour s m = case m of
	U -> if i<=0 then Nothing else Just (move s m)
	D -> if i>=n-1 then Nothing else Just (move s m)
	L -> if j<=0 then Nothing else Just (move s m)
	R -> if j>=n-1 then Nothing else Just (move s m)
	where
		(i,j)= one2two n (emptyTile s)
		n = dimension s

--A function that returns the possible states that we can go to from a particular state
neighbours :: State -> [State]
neighbours s = mapMaybe (neighbour s) [U,D,L,R]

--The main game AI. it takes in a state and returns a final state. We can go prev on the final state to get the sequence of steps to solve the input state
solve :: State -> State
solve s = go (PQ.fromList [(distKey s, s)])
	where
    go fr = if distKey state == 0 
            then state 
            else go fr2
            where
	        -- Retrieve the game state with the lowest priority and remove it from
	        -- the frontier.
	        ((_, state), fr1) = PQ.deleteFindMin fr

	        -- If the new board is the same as the previous board then
	        -- do not add it to the queue since it has already been explored.
	        ns = case previous state of
	          Nothing -> neighbours state
	          Just n  -> filter (\x -> board x /= board n) (neighbours state)

	        -- The priority of a puzzle is the number of moves so far
	        -- plus the manhattan distance.
	        ps  = zip [moves q + distKey q | q <- ns] ns
	        fr2 = foldr (uncurry PQ.insert) fr1 ps

--A function that returns the list of states that the puzzle goes into as we solve it
boards :: State -> [[Int]]
boards s = map Vector.toList (reverse (bords s))
  where
    bords s = case previous s of
      Nothing -> [board s]
      Just r  -> board s : bords r

--A function to cretate a list of Int from the input string
fromString :: String -> [[Int]]
fromString s = (map . map) read ws
  where ws = map words (lines s)

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


main = do
  putStrLn "Enter the name of the file containing the puzzle specification: "
  txt <- readFile =<< getLine
  let game = fromString txt
      ([n], brd) = case game of
        [] -> error "Invalid puzzle file"
        x:xs -> (x, concat xs)
  let p = solve (createState brd)
  printer (boards p)