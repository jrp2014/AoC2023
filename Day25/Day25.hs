module Main where

import Data.Function (on)
import Data.Graph.Inductive
import Data.List (delete, maximumBy, nub, (\\))
import Data.Maybe (fromJust)
import Data.Tuple (swap)

-- inspired by https://www.reddit.com/r/adventofcode/comments/18qbsxs/comment/ketzp94/?utm_source=share&utm_medium=web2x&context=3
-- Could probably be done with a Map from Component to Set of Component, rather than a Graph

type Input = [(Component, [Component])]

type Component = String

type Connection = (String, String)

-- The graph does not need to be labelled to solve the problem, but it's handy for debugging
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

solve :: Diagram -> Int
solve g = group1size * (order g - group1size)
  where
    group1 :: [Node]
    group1 = loop (nodes g)

    group1size = length group1 :: Int

    loop :: [Node] -> [Node]
    loop s = if sum (map count s) == 3 then s else loop (delete (maximumBy (compare `on` count) s) s)
      where
        count :: Node -> Int
        count v = length (suc g v \\ s)

main :: IO ()
main = do
  input <- parse . map words . lines <$> readFile "test.txt"

  -- print $ nds input
  -- print $ egs input

  let diagram = mkDiagram (nds input) (egs input)

  -- putStrLn $ prettify diagram

  -- print $ order diagram
  print $ solve diagram
