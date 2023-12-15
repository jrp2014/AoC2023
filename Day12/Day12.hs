{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/12>

>>> :{
:main +
"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"
:}
21
525152

-}
module Main (main) where

import Control.Applicative (Alternative(empty))
import Data.Array (Ix(range), (!), listArray)
import Data.List (intercalate)
import Data.Array.Unboxed qualified as A
import Data.Array.Base qualified as AB
import GHC.Arr qualified as GA

-- |
--
-- >>> :main
-- 6871
-- 2043098029844
main :: IO ()
main =
 do raw <- readFile "input.txt"
    let input = map (\ (xs, ys) -> (xs, read ('[' : ys ++ "]"))) . break (== ' ')$ lines raw
    print (sum [ways g s | (s,g) <- input])
    print (sum [ways (concat (replicate 5 g)) (unfoldSprings s) | (s,g) <- input])

unfoldSprings :: String -> String
unfoldSprings = intercalate "?" . replicate 5

ways :: [Int] -> [Char] -> Int
ways groups springs = answersA ! (0,0)
  where
    groupsN = length groups
    groupsA = listArray (0, groupsN - 1) groups

    springsN = length springs
    springsA = listArray (0, springsN - 1) springs

    answersB = ((0,0),(groupsN,springsN))
    answersA = listArray answersB [go i j | (i,j) <- range answersB]

    go :: Int -> Int -> Int
    go groupI springI =
      let dotCase  = answersA ! (groupI, springI + 1)
          hashCase = startGroup groupI (springI + 1)
          {-# Inline hashCase #-}
      in case arrIx springsA springI of
        Just '.' -> dotCase
        Just '#' -> hashCase
        Just '?' -> hashCase + dotCase
        Nothing | groupI == groupsN -> 1
        _                           -> 0

    startGroup:: Int -> Int -> Int
    startGroup groupI springI =
      case arrIx groupsA groupI of
        Just n  -> goGroup (groupI + 1) (n - 1) springI
        Nothing -> 0

    goGroup :: Int -> Int -> Int -> Int
    goGroup groupI n springI =
      let doneCase = answersA ! (groupI, springI + 1)
          moreCase = goGroup groupI (n-1) (springI + 1)
          {-# Inline moreCase #-}
      in case arrIx springsA springI of
        Just '.' | n == 0    -> doneCase
        Just '#' | n >  0    -> moreCase
        Just '?' | n == 0    -> doneCase
                 | otherwise -> moreCase
        Nothing  | n == 0, groupI == groupsN -> 1
        _                                    -> 0


-- | Index an array returning 'Nothing' if the index is out of bounds.
arrIx :: (A.IArray a e, A.Ix i, Alternative f) => a i e -> i -> f e
arrIx a i
  | A.inRange b i = pure $! AB.unsafeAt a (GA.unsafeIndex b i)
  | otherwise = empty
  where b = A.bounds a
{-# Inline arrIx #-}

