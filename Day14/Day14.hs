{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Main
-- Description : Day 14 solution
-- Copyright   : (c) Eric Mertens, 2023
-- License     : ISC
-- Maintainer  : emertens@gmail.com
--
-- <https://adventofcode.com/2023/day/14>
--
-- >>> :{
-- :main +
-- "O....#....
-- O.OO#....#
-- .....##...
-- OO.#O....O
-- .O.....O#.
-- O.#..O.#.#
-- ..O..#O..O
-- .......O..
-- #....###..
-- #OO..#....
-- "
-- :}
-- 136
-- 64
module Main (main) where

import Data.List (elemIndices, intercalate, sortBy, transpose)
import Data.List.Split (splitOn)
import Data.Map qualified as Map

cmp :: Char -> Char -> Ordering
cmp 'O' '.' = LT
cmp '.' 'O' = GT
cmp _ _ = EQ

-- |
--
-- >>> :main
-- 109596
-- 96105
main :: IO ()
main =
  do
    raw <- readFile "input.txt"
    let input = transpose (lines raw)

    print (load (map shift input))
    print (load (map settle input))

    let process = times 4 (transpose . map (reverse . shift))
        outs = iterate process input
        (start, next) = findCycle outs
        i = start + (1_000_000_000 - start) `rem` (next - start)
    print (load (outs !! i))

-- | Compute the load on the north support beams
load :: [String] -> Int
load = sum . map weight
  where
    weight xs = sum [n - w | w <- elemIndices 'O' xs]
      where
        n = length xs

-- | Shift the rocks on a single row to the left
shift :: String -> String
shift = go 0
  where
    go n ('.' : xs) = go (n + 1) xs
    go n ('O' : xs) = 'O' : go n xs
    go n ('#' : xs) = replicate n '.' ++ '#' : go 0 xs
    go n _ = replicate n '.'

-- | Same as shift, but easier to grok
settle :: String -> String
settle = intercalate "#" . map (sortBy cmp) . splitOn "#"

-- | Report the first and second index a duplicate element
-- is found in the list.
findCycle :: (Ord a) => [a] -> (Int, Int)
findCycle = go Map.empty 0
  where
    go _ _ [] = error "no cycle"
    go seen i (x : xs) =
      case Map.lookup x seen of
        Nothing -> go (Map.insert x i seen) (i + 1) xs
        Just j -> (j, i)

-- | Apply a function @n@ times strictly.
times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0 = x
  | otherwise = times (n - 1) f $! f x
{-# INLINE times #-}
