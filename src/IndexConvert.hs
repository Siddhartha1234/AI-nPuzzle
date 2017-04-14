module IndexCovert where

--A function that converts the indices of a 2D array to the corresponding indice in a 1D array
two2one :: Int -> Int -> Int -> Int
two2one n i j = n * i + j

--A function that converts the indices of a 1D array to the corresponding indices in a 2D array
one2two :: Int -> Int -> (Int, Int)
one2two n a = (a `div` n, a `mod` n)