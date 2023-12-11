{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Main
-- Description : Day 11 solution
-- Copyright   : (c) Eric Mertens, 2023
-- License     : ISC
-- Maintainer  : emertens@gmail.com
--
-- <https://adventofcode.com/2023/day/11>
--
-- >>> :{
-- :main +
-- "...#......
-- .......#..
-- #.........
-- ..........
-- ......#...
-- .#........
-- .........#
-- ..........
-- .......#..
-- #...#.....
-- "
-- :}
-- 374
-- 82000210
module Main where

import Data.Foldable qualified as F
import Data.Map qualified as Map
import Data.Map.Strict as SMap

-- | Two-dimensional coordinate
data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq)

-- | Given a list of lines pair up each character with
-- its position.
coordLines :: [String] -> [(Coord, Char)]
coordLines rows = [(C y x, z) | (y, row) <- zip [0 ..] rows, (x, z) <- zip [0 ..] row]

-- | Compute the number of occurrences of the elements in a given list.
--
-- >>> counts "bababc"
-- fromList [('a',2),('b',3),('c',1)]
counts :: (Foldable f, Ord a) => f a -> Map a Int
counts xs = SMap.fromListWith (+) [(x, 1) | x <- F.toList xs]

-- |
--
-- >>> :main
-- 9965032
-- 550358864332
main :: IO ()
main =
  do
    raw <- readFile "input.txt"
    let input = lines raw
    let galaxies = [c | (c, '#') <- coordLines input]
        rows = counts [y | C y _ <- galaxies]
        cols = counts [x | C _ x <- galaxies]
        solve n = solve1 n rows + solve1 n cols
    print (solve 2)
    print (solve 1_000_000)

-- | Solve the travel problem along a single axis
solve1 ::
  -- | expansion factor
  Int ->
  -- | map from location to number of galaxies
  Map Int Int ->
  -- | total distance traveled
  Int
solve1 ex m =
  let total = sum m
   in case Map.assocs m of
        [] -> 0
        (here, n) : xs -> solve1' ex n (total - n) 0 here xs

solve1' ::
  -- | expandsion factor
  Int ->
  -- | galaxies to the left
  Int ->
  -- | galaxies to the right
  Int ->
  -- | accumulator
  Int ->
  -- | current location
  Int ->
  -- | remaining locations and counts
  [(Int, Int)] ->
  -- | total distances traveled
  Int
solve1' _ _ _ acc _ [] = acc
solve1' ex l r acc here ((there, m) : xs) =
  solve1' ex (l + m) (r - m) (acc + crossings * distance) there xs
  where
    crossings = l * r
    distance = (there - here - 1) * ex + 1
