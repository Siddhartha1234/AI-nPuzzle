{-# LANGUAGE TupleSections #-}
import Control.Applicative
import Control.Monad hiding (mapM_)
import Control.Arrow
import Data.Foldable
import Data.Maybe
import Data.List (delete)
import Data.Ix
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Prelude hiding
    ( minimum
    , concat
    , notElem
    , concatMap
    , all
    , mapM_
    )

type Coord = (Int, Int)
type Shape = S.Set Coord
type Block = (Coord, Shape)
type Box = M.Map String Block
type Move = (String, (Coord, Coord))

data Puzzle = Puzzle
    { pzM           :: Int    -- ^ puzzle size MxN
    , pzN           :: Int
    , pzBox         :: Box    -- ^ initial box
    , targetBlkName :: String
    , targetCoord   :: Coord
    } deriving Show

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons ~(a:as) = Just (a,as)

readPairLn :: (Read a, Read b) => IO (a,b)
readPairLn = do
    (a:b:_) <- words <$> getLine
    return (read a, read b)

readPuzzle :: IO Puzzle
readPuzzle = do
    (m,n) <- readPairLn
    b <- readBox m n
    Puzzle m n b <$> getLine    -- target block
                 <*> readPairLn -- target coord

-- | convert from a list of coordinates
--   to a block
coordsToBlock :: [Coord] -> Block
coordsToBlock xs = (topLeft, S.fromList . map (subtract x *** subtract y) $ xs)
    where
      topLeft@(x,y) = (minimum . map fst &&& minimum . map snd) xs

-- | convert from a block
--   to a list of coordinates
blockToCoords :: Block -> [Coord]
blockToCoords ((x,y), cs) = map ((+ x) *** (+ y)) $ S.toList cs

-- | read and parse initial box from input
readBox :: Int -> Int -> IO Box
readBox m n = toBox . concat <$> forM [0..m-1] readRow
    where
      readRow :: Int -> IO [(String, Coord)]
      -- get every single cell name paired with its coordinate
      readRow row = zipWith (\col w -> (w,(row,col))) [0..n-1]
                  . words <$> getLine
      toBox :: [(String, Coord)] -> Box
      toBox = fmap coordsToBlock
            . M.fromListWith (++)         -- cluster coordinates by names
            . map (second (:[]))
            . filter (notElem  '.' . fst) -- dots are empty cells

-- | unsafe map lookup
find' :: Ord a => a -> M.Map a b -> b
find' x = fromJust . M.lookup x

-- | perform BFS to solve the puzzle
search :: Puzzle
       -> [(Box,[Move])]            -- ^ BFS queue
       -> S.Set (Coord,S.Set Block) -- ^ visited states (digested)
       -> (Box,[Move])              -- ^ return one solution
search _ [] _ = error "solution not found"
search puz ((b,mvs):bs) visited
      -- skipping visited states
    | digestBox b `S.member` visited = search puz bs visited
      -- a solution is found
    | fst targetBlock == targetCoord puz = (b,mvs)
      -- expand the current queue, and search next one in the queue
    | otherwise = search puz (bs ++ nextMoves (pzM puz) (pzN puz) (b,mvs)) newVisited
    where
      newVisited = S.insert (digestBox b) visited
      targetBlock = find' (targetBlkName puz) b
      -- the state is digested by wiping out all block names
      -- but keep the coordinate of the target block
      -- in case the target block happens to have the same shape
      -- of a non-target block
      digestBox :: Box -> (Coord, S.Set Block)
      digestBox b' = ( fst targetBlock
                     , S.fromList . M.elems $ b')

-- | given one state with its move history
--   get all possibilities of its next state
nextMoves :: Int -> Int -> (Box,[Move]) -> [(Box,[Move])]
nextMoves m n (b,mvs) = do
    -- select one block
    blkName  <- case uncons mvs of
        Nothing -> M.keys b
        -- but if we have a previous move, exclude that move
        -- so we don't end up with cycles
        Just ((mv,_),_) -> delete mv (M.keys b)
    let curBlock = find' blkName b
        -- remove the selected block from box
        remainingBox = M.delete blkName b
        -- and get a list of coordinates of occupied cells
        remainingCoords = concatMap blockToCoords . M.elems $ remainingBox
        -- do another BFS to get a list of possibilities reached
        -- by just moving this selected block (expand)
        expand :: [Block] -> S.Set Block -> S.Set Block
        expand [] visited = visited
        expand (nxtBlk:nxtBlks) visited
              -- if the current block possition has been visited
            | nxtBlk `S.member` visited = expand nxtBlks visited
              -- if all coordinates are valid (see below)
              -- then mark it as visited and expand the queue
            | all validCoord nxtCoords
                = expand (nxtBlks ++ map (,snd nxtBlk) (nextCoords . fst $ nxtBlk))
                         (S.insert nxtBlk visited)
              -- not a valid block position, try next one
            | otherwise = expand nxtBlks visited
            where
              nxtCoords = blockToCoords nxtBlk
              -- a valid block should be:
              validCoord = (&&) <$> inRange ((0,0),(m-1,n-1))   -- within range
                                <*> (`notElem` remainingCoords) -- taking only empty cells
    -- get all possible moves using this selected block
    map (\nxtBlk -> ( M.insert blkName nxtBlk remainingBox -- plug in this block
                    ,(blkName ,( fst curBlock              -- update move history
                               , fst nxtBlk) ):mvs))
      . S.toList
      $ expand [curBlock] S.empty

-- | all possible next coordinates
nextCoords :: Coord -> [Coord]
nextCoords (x,y) = map ((+ x) *** (+ y)) [(-1,0), (1,0), (0,-1), (0,1)]

-- | pretty print a move history
printMove :: Move -> IO ()
printMove (s,(c1,c2)) = putStrLn $ unwords [s, show c1, show c2]

main :: IO ()
main = do
    pz <- readPuzzle
    let moves = reverse
              . snd
              $ search pz [(pzBox pz,[])] S.empty
    print (length moves)
    mapM_ printMove moves