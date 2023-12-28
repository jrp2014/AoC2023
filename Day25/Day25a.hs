module Main where

import Data.Function (on)
import Data.Graph.Inductive
import Data.List (delete, maximumBy, nub, (\\))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Debug.Trace

-- inspired by https://www.reddit.com/r/adventofcode/comments/18qbsxs/comment/ketzp94/?utm_source=share&utm_medium=web2x&context=3
-- Could probably be done with a Map from Component to Set of Component, rather than a Graph

type Input = [(Component, [Component])]

type Component = String

type Connection = (String, String)

type Components = Set Component

type Diagram = Map Component Components

parse :: [[String]] -> Input
parse = map (\ws -> (init $ head ws, tail ws))

-- Diagram
mkDiagram :: Input -> Diagram
mkDiagram input =
  Map.unionWith
    Set.union
    ( Map.fromList
        [(from, Set.fromList tos) | (from, tos) <- input]
    )
    ( Map.fromList
        [(to, Set.singleton from) | (from, tos) <- input, to <- tos]
    )

-- solve :: Diagram -> Int
solve diagram = Set.size group1 * Set.size ((Map.keysSet diagram) Set.\\ group1)
  where
    group1 :: Components
    group1 = loop (Map.keysSet diagram)

    loop :: Components -> Components
    -- loop s = traceShow (map count (Set.toList s)) $ if sum (map count (Set.toList s)) == 3 then s else loop (Set.delete (maximumBy (compare `on` count) s) s)
    loop s = if sum (map count (Set.toList s)) == 3 then s else loop (Set.delete (maximumBy (compare `on` count) s) s)
      where
        count :: Component -> Int
        count c = Set.size (diagram Map.! c Set.\\ s)

main :: IO ()
main = do
  input <- parse . map words . lines <$> readFile "test.txt"

  print input

  let diagram = mkDiagram input

  print diagram
  print $ length diagram

  print $ solve diagram
