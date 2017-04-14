module BoardProps where



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