main = do
  putStrLn "Enter the name of the file containing the puzzle specification: "
  txt <- readFile =<< getLine
  let game = fromString txt
      ([n], brd) = case game of
        [] -> error "Invalid puzzle file"
        x:xs -> (x, concat xs)
  let p = solve (createState brd)
  printer (boards p)