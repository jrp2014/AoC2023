module Day05 where

-- https://gist.github.com/mhitza/c3b6de8a283c920daf01c3d559812d75#file-day6-hs

import Data.Char
import Data.Foldable
import Data.Monoid


main :: IO ()
main = do
  input <- (map words . lines) <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input

part1 :: [[String]] -> Int
part1 = product . map waysToBeat . parse
  where
    parse [time, distance] = zip (toNumbers time) (toNumbers distance)
    toNumbers = map read . drop 1

part2 :: [[String]] -> Int
part2 = waysToBeat . parse
  where
    parse [time, distance] = (toNumber time, toNumber distance)
    toNumber = read . filter isDigit . concat . drop 1

waysToBeat :: (Int, Int) -> Int
waysToBeat (time, distance) = first (time, time - 1, 1) - first (1, 2, time) + 1
  where
    first = maybe 0 id . getFirst . findFirstIn
    findFirstIn (s, s', e) =
      fold
        [ First (Just hold)
          | hold <- [s, s' .. e],
            let remaining = time - hold,
            remaining * hold > distance
        ]
