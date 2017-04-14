module BoardIO where

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