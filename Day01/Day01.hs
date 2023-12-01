module Day01 where

import Data.Char

parse :: String -> [[Int]]
parse = map (map digitToInt . filter isDigit) . lines

solve :: [[Int]] -> Int
solve = sum . map (\row -> 10 * head row + last row)

parse2 :: String -> [[Int]]
parse2 = map translateLine . lines
  where
    translateLine :: [Char] -> [Int]
    translateLine [] = []
    translateLine ('o' : 'n' : 'e' : xs) = 1 : translateLine ('e' : xs)
    translateLine ('t' : 'w' : 'o' : xs) = 2 : translateLine ('o' : xs)
    translateLine ('t' : 'h' : 'r' : 'e' : 'e' : xs) = 3 : translateLine ('e' : xs)
    translateLine ('f' : 'o' : 'u' : 'r' : xs) = 4 : translateLine xs
    translateLine ('f' : 'i' : 'v' : 'e' : xs) = 5 : translateLine ('e' : xs)
    translateLine ('s' : 'i' : 'x' : xs) = 6 : translateLine xs
    translateLine ('s' : 'e' : 'v' : 'e' : 'n' : xs) = 7 : translateLine ('n' : xs)
    translateLine ('e' : 'i' : 'g' : 'h' : 't' : xs) = 8 : translateLine ('t' : xs)
    translateLine ('n' : 'i' : 'n' : 'e' : xs) = 9 : translateLine ('e' : xs)
    translateLine (x : xs) = if isNumber x then digitToInt x : translateLine xs else translateLine xs

main :: IO ()
main = do
  input <- readFile "input.txt"

  putStrLn "Part1:"
  print $ solve (parse input)

  putStrLn "Part2:"
  print $ solve (parse2 input)
