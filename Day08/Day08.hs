{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Main
-- Description : Day 8 solution
-- Copyright   : (c) Eric Mertens, 2023
-- License     : ISC
-- Maintainer  : emertens@gmail.com
--
-- <https://adventofcode.com/2023/day/8>
module Day08 where

import Control.Monad (unless)
import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

data D = L | R deriving (Read)

parse :: [String] -> (String, [(String, String, String)])
parse lns = (instructions, kslsrs)
  where
    instructions = head lns
    kslsrs = map (toTuple . words) $ drop 2 lns
    toTuple (key : _ : left : right : _) = (key, strip left, strip right)
      where
        strip = filter (\x -> x /= '(' && x /= ',' && x /= ')')

part1, part2 :: String -> Bool
part1 x = "ZZZ" == x
part2 x = 'Z' == last x

main :: IO ()
main = do
  input <- readFile "inpuT.txt"
  let (steps, nodes) = parse . lines $ input

  let nodes' = Map.fromList [(k, \case L -> a; R -> b) | (k, a, b) <- nodes]
  let steps' = map (\step -> read [step]) steps

  let mkPath start = scanl (nodes' Map.!) start (cycle steps')

  let path1 = mkPath "AAA"
  let paths2 = [mkPath start | (start, _, _) <- nodes, last start == 'A']

  print $ (findIndex' part1 path1)

  unless (all (isTrivial part2 (length steps)) paths2) (fail "input not trivial")

  print $ (foldl1 lcm (map (findIndex' part2) paths2))

-- Verifies that we actually got one of the trivial input files.

-- * The goal must be reached after a number of cycles that is a multiple of the steps

-- * The next goal must be the same as the previous and must be reachable in the

--   same number of steps
--
-- This guarantees that the path must actually cycle infinitely and that
-- there is exactly one goal state in the cycle.
isTrivial :: (Eq a) => (a -> Bool) -> Int -> [a] -> Bool
isTrivial p n xs =
  case [(i, x) | (i, x) <- zip [0 ..] xs, p x] of
    (i1, g1) : (i2, g2) : _ -> i1 `rem` n == 0 && 2 * i1 == i2 && g1 == g2
    _ -> False

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' p = fromJust . findIndex p
