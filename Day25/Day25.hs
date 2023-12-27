module Day25 where

-- import Data.Containers.ListUtils (nubOrd)

import Data.Function (on)
import Data.Graph.Inductive
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Query.MaxFlow
import Data.List (delete, intersect, maximumBy, nub, (\\))
import Data.Maybe
import qualified Data.Set as S
import Data.Tuple

type Input = [(Component, [Component])]

type Component = String

type Connection = (String, String)

type Diagram = Gr String String

parse :: [[String]] -> Input
parse = map (\ws -> (init $ head ws, tail ws))

-- nodes
nds :: Input -> [Component]
nds input = nub [n | (from, tos) <- input, n <- from : tos]

-- edges
egs :: Input -> [Connection]
egs input = [(from, to) | (from, tos) <- input, to <- tos]

-- Diagram
mkDiagram :: [Component] -> [Connection] -> Diagram
mkDiagram n e = undir $ mkGraph (map swap n') e'
  where
    n' = zip n [1 ..]
    e' = map (\(from, to) -> (fromJust $ lookup from n', fromJust $ lookup to n', from ++ " " ++ to)) e

type NodeSet = S.Set Node

solve :: Diagram -> [Node]
solve g = loop (nodes g)
  where
    loop :: [Node] -> [Node]
    loop s = if sum (map count s) == 3 then s else loop (delete (maximumBy (compare `on` count) s) s)
      where
        count :: Node -> Int
        count v = length (suc g v \\ s)

main :: IO ()
main = do
  input <- parse <$> map words . lines <$> readFile "input.txt"

  print $ nds input
  print $ egs input

  let gr = mkDiagram (nds input) (egs input)

  putStrLn $ prettify gr

  let lsolution = length $ solve gr

  print $ order gr
  print $ (lsolution * (order gr - lsolution))
