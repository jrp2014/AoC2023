{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TransformListComp #-}

-- |
--
-- Based on
--
-- Module      : Main
-- Description : Day 7 solution
-- Copyright   : (c) Eric Mertens, 2023
-- License     : ISC
-- Maintainer  : emertens@gmail.com
--
-- <https://adventofcode.com/2023/day/7>
--
-- >>> :{
-- :main +
-- "32T3K 765
-- T55J5 684
-- KK677 28
-- KTJJT 220
-- QQQJA 483
-- "
-- :}
-- 6440
-- 5905

module Day7 where

import Data.Foldable (toList)
import Data.List (elemIndex, nub, sort, sortOn)
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Map.Strict qualified as SMap

-- |
--
-- >>> :main
-- 248422077
-- 249817836
main :: IO ()
main =
  do
    input <- readFile "input.txt"
    let pinput = parse input

    print (winnings strength1 pinput)
    print (winnings strength2 pinput)

parse :: String -> [(String, Int)]
parse = map (hb . words) . lines
  where
    hb :: [String] -> (String, Int)
    hb [hand, bid] = (hand, read bid)

winnings :: (Ord a) => (String -> a) -> [(String, Int)] -> Int
winnings strength input =
  sum
    [ bid * rank
      | rank <- [1 ..]
      | (hand, bid) <- input,
        then sortOn by strength hand
    ]

strength1 :: String -> [Int]
strength1 hand = category hand : map val hand
  where
    val x = fromJust (x `elemIndex` "23456789TJQKA")

strength2 :: String -> [Int]
strength2 hand =
  maximum
    [ category (map rpl hand) : map val hand
      | alt <- nub hand,
        let rpl x = if x == 'J' then alt else x
    ]
  where
    val x = fromJust (x `elemIndex` "J23456789TQKA")

category :: String -> Int
category hand =
  case sort (toList (counts hand)) of
    [5] -> 6
    [1, 4] -> 5
    [2, 3] -> 4
    [1, 1, 3] -> 3
    [1, 2, 2] -> 2
    [1, 1, 1, 2] -> 1
    [1, 1, 1, 1, 1] -> 0
    _ -> error "bad hand"

-- | Compute the number of occurrences of the elements in a given list.
--
-- >>> counts "bababc"
-- fromList [('a',2),('b',3),('c',1)]
counts :: (Foldable f, Ord a) => f a -> Map a Int
counts xs = SMap.fromListWith (+) [(x,1) | x <- toList xs]
