{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Main
-- Description : Day 15 solution
-- Copyright   : (c) Eric Mertens, 2023
-- License     : ISC
-- Maintainer  : emertens@gmail.com
--
-- <https://adventofcode.com/2023/day/15>
--
-- >>> :main + "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n"
-- 1320
-- 145
module Main where

import Data.Array (accumArray, assocs)
import Data.Char (ord)
import Data.List.Split (splitOn)

parse :: String -> [String] -- [(String, String)]
parse = splitOn "," . head . lines

parseCmd :: String -> (String, Maybe Int)
parseCmd s = (lbl, parseCmd' cmd)
  where
    (lbl, cmd) = span (`notElem` "=-") s
    parseCmd' ('=' : cmd') = Just $ read cmd'
    parseCmd' ("-") = Nothing

-- |
--
-- >>> :main
-- 503487
-- 261505
main :: IO ()
main =
  do
    raw <- readFile "input.txt"
    let input = parse raw
    print $ sum (map hasher input)

    let pinput = map parseCmd input
    let boxes =
          accumArray -- combines existing entries using apply
            apply
            []
            (0, 255)
            [(hasher lbl, (lbl, cmd)) | (lbl, cmd) <- pinput]

    print $
      sum
        [ (1 + box) * i * len
          | (box, xs) <- assocs boxes,
            (i, (_, len)) <- zip [1 ..] xs
        ]

hasher :: String -> Int
hasher str = foldl (\acc x -> 17 * (ord x + acc)) 0 str `rem` 256

apply :: [(String, Int)] -> (String, Maybe Int) -> [(String, Int)]
apply prev (lbl, Nothing) = filter ((lbl /=) . fst) prev -- remove lbl from prev
apply prev (lbl, Just n) = go prev -- add lbl to prev
  where
    go ((k, _) : xs) | lbl == k = (lbl, n) : xs -- replace the old lens
    go (x : xs) = x : go xs
    go [] = [(lbl, n)] -- add the lens
