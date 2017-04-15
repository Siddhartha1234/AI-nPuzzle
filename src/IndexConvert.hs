module IndexConvert where

import Data.Char
--A function that converts the indices of a 2D array to the corresponding indice in a 1D array
two2one :: Int -> Int -> Int -> Int
two2one n i j = n * i + j

--A function that converts the indices of a 1D array to the corresponding indices in a 2D array
one2two :: Int -> Int -> (Int, Int)
one2two n a = (a `div` n, a `mod` n)

int_to_string :: Int -> String 
int_to_string i = if i < 10 then [intToDigit i] else (int_to_string (i `div` 10)) ++ (int_to_string (i `mod` 10))