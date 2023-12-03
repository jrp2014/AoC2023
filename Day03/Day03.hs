{-# Language ImportQualifiedPost #-}
{-|
Module      : Day 3
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/3>

>>> :{
:main +
"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."
:}
4361
467835

-}
module Day03 where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as SMap
import Data.Char (isDigit)
import Data.List (find)
import Data.Foldable (toList)

type Part = Int

parse :: String -> Map Coord Char
parse = SMap.fromList . coordLines . lines

-- | Parse the input schematic and print answers to both parts.
--
-- >>> :main
-- 538046
-- 81709807
main :: IO ()
main =
 do raw <- readFile "input.txt"
    let input = parse raw

        lkp :: Coord -> Char
        lkp i = Map.findWithDefault '.' i input

        -- Map of each part in the schematic to the list of adjacent part numbers
        partMap :: Map Coord [Part]
        partMap = Map.fromListWith (++)
          [ (part, [read (map lkp cs)])
          | (c,n) <- Map.assocs input
          , isDigit n
          , not (isDigit (lkp (left c)))
          , let cs = takeWhile (isDigit . lkp) (iterate right c)
          , Just part <- [find (isSymbol . lkp) (concatMap neighbors cs)]
          ]

    print (sum (fmap sum partMap))
    print (sum [a * b | (c, [a,b]) <- Map.assocs partMap, '*' == lkp c])

-- | Things that aren't digits or periods.
isSymbol :: Char -> Bool
isSymbol x = not (isDigit x) && x /= '.'



-- | Two-dimensional coordinate
data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq)

-- | Row (y) of coordinate
coordRow :: Coord -> Int
coordRow (C row _) = row

-- | Column (x) of coordinate
coordCol :: Coord -> Int
coordCol (C _ col) = col

-- | Decrement y coordinate
above :: Coord -> Coord
above (C y x) = C (y-1) x

-- | Increment y coordinate
below :: Coord -> Coord
below (C y x) = C (y+1) x

-- | Decrement x coordinate
left :: Coord -> Coord
left  (C y x) = C y (x-1)

-- | Increment x coordinate
right :: Coord -> Coord
right (C y x) = C y (x+1)

-- | Swap x and y coordinates
invert :: Coord -> Coord
invert (C y x) = C x y

-- | Swap x and y coordinates
invert' :: Coord -> Coord
invert' (C y x) = C (-x) (-y)

-- | Invert the x coordinate
flipX :: Coord -> Coord
flipX (C y x) = C y (-x)

-- | Invert the y coordinate
flipY :: Coord -> Coord
flipY (C y x) = C (-y) x

-- | Rotate coordinate 90-degrees CCW about the origin
turnLeft :: Coord -> Coord
turnLeft  (C y x) = C (-x) y

-- | Rotate coordinate 90-degrees CW about the origin
turnRight :: Coord -> Coord
turnRight (C y x) = C x (-y)

-- | Rotate the coordinate 180-degrees about the origin
turnAround :: Coord -> Coord
turnAround (C y x) = C (-y) (-x)

-- | Compute the Manhattan distance between two coordinates
manhattan :: Coord -> Coord -> Int
manhattan a b = norm1 (a - b)

-- | Compute 1-norm between two coordinates (sum of magnitudes)
norm1 :: Coord -> Int
norm1 (C y x) = abs y + abs x

-- | Compute infinity-norm between two coordinates (max of magnitudes)
normInf :: Coord -> Int
normInf (C y x) = max (abs y) (abs x)

-- | Compute the 4 cardinal neighbors of a coordinate: north, south, east, west
cardinal :: Coord -> [Coord]
cardinal c = c `seq` [above c, left c, right c, below c]

-- | Compute the 8 cardinal neighbors and diagonal neighbors
neighbors :: Coord -> [Coord]
neighbors c = c `seq` [above c, left c, right c, below c,
                       above (left c), above (right c),
                       below (left c), below (right c)]

-- | Find the upper-left and lower-right coordinates that
-- inclusively contain all the coordinates in a list of
-- coordinates.
boundingBox :: Foldable f => f Coord -> Maybe (Coord, Coord)
boundingBox t =
  case toList t of
    []         -> Nothing
    C y x : cs -> go y x y x cs
  where
    go loy lox hiy hix [] = lo `seq` hi `seq` Just (lo, hi)
      where
        lo = C loy lox
        hi = C hiy hix
    go loy lox hiy hix (C y x : cs) = go (min loy y) (min lox x) (max hiy y) (max hix x) cs

-- | Coordinate at the origin
origin :: Coord
origin = C 0 0

-- | Unit vector pointing up
north :: Coord
north = C (-1) 0

-- | Unit vector pointing right
east :: Coord
east = C 0 1

-- | Unit vector pointing down
south :: Coord
south = C 1 0

-- | Unit vector pointing left
west :: Coord
west = C 0 (-1)

-- | Scale a coordinate as a vector from the origin
scaleCoord :: Int -> Coord -> Coord
scaleCoord n = mapCoord (n *)

-- | Render a minimal bounding box containing all the characters
-- at the given coordinates. Empty space filled with space characters.
drawPicture :: Map Coord Char -> String
drawPicture pixels =
  case boundingBox (Map.keys pixels) of
    Nothing -> ""
    Just (C miny minx, C maxy maxx) ->
      unlines [[Map.findWithDefault '·' (C y x) pixels | x <- [minx .. maxx]] | y <- [miny .. maxy]]

-- | Render a minimal bounding box containing boxes
-- at the given coordinates.
drawCoords :: Foldable t => t Coord -> String
drawCoords coords = drawPicture (Map.fromList [(c,'█') | c <- toList coords])

-- | Given a list of lines pair up each character with
-- its position.
coordLines :: [String] -> [(Coord, Char)]
coordLines rows = [(C y x, z) | (y,row) <- zip [0..] rows, (x,z) <- zip [0..] row]

-- | Apply a function to the y and x coordinate
mapCoord :: (Int -> Int) -> Coord -> Coord
mapCoord f (C y x) = C (f y) (f x)

-- | Use a function pairwise on x and y coordinates of the two arguments
zipCoord :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
zipCoord f (C y1 x1) (C y2 x2) = C (f y1 y2) (f x1 x2)

-- | Generate a unit vector corresponding to the arrow symbol: @^v<>@
charToVec :: Char -> Maybe Coord
charToVec '^' = Just north
charToVec 'v' = Just south
charToVec '>' = Just east
charToVec '<' = Just west
charToVec  _  = Nothing

-- | Vector arithmetic
instance Num Coord where
  (+) = zipCoord (+)
  {-# INLINE (+) #-}
  (-) = zipCoord (-)
  {-# INLINE (-) #-}
  (*) = zipCoord (*)
  {-# INLINE (*) #-}
  negate = mapCoord negate
  {-# INLINE negate #-}
  abs = mapCoord abs
  {-# INLINE abs #-}
  signum = mapCoord signum
  {-# INLINE signum #-}
  fromInteger = (\i -> C i i) . fromInteger
  {-# INLINE fromInteger #-}

