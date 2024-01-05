module Day19 where

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

data Category = CX | CM | CA | CS deriving (Show)

data Result = Accepted | Rejected | Next String deriving (Show)

data Condition = Condition {category :: Category, o :: Ordering, bound :: Int, destination :: Result} | Default Result deriving (Show)

data Rule = Rule {rulename :: String, conditions :: [Condition]} deriving (Show)

type WorkFlow = [Rule]

data Rating = Rating {cx, cm, ca, cs :: Int} deriving (Show)

parseInput :: Parser (WorkFlow, [Rating])
parseInput = do
  rules <- parseRule `endBy` newline
  _ <- newline
  ratings <- parseRating `endBy` newline
  eof
  pure (rules, ratings)

parseRule :: Parser Rule
parseRule = do
  l <- identifier
  cs <- between (char '{') (char '}') (parseCondition `sepBy` char ',')
  pure $ Rule l cs

parseCondition :: Parser Condition
parseCondition =
  (do _ <- char 'A'; pure (Default Accepted))
    <|> (do _ <- char 'R'; pure (Default Rejected))
    <|> try parseCondition'
    <|> (Default . Next <$> identifier)
    <?> "Failed to parse condition"

parseCondition' :: Parser Condition
parseCondition' = do
  c <- lower
  o <- choice [char '<', char '>']
  d <- decimal
  _ <- char ':'
  Condition (toCategory c) (if o == '<' then LT else GT) d . toResult <$> identifier

parseRating :: Parser Rating
parseRating = between (char '{') (char '}') parseRating'

parseRating' :: Parser Rating
parseRating' = do
  _ <- string "x="
  dx <- decimal
  _ <- string ",m="
  dm <- decimal
  _ <- string ",a="
  da <- decimal
  _ <- string ",s="
  Rating dx dm da <$> decimal

identifier :: Parser String
identifier = many1 letter

decimal :: Parser Int
decimal = read <$> many1 digit

toCategory :: Char -> Category
toCategory 'x' = CX
toCategory 'm' = CM
toCategory 'a' = CA
toCategory 's' = CS

toResult :: String -> Result
toResult "A" = Accepted
toResult "R" = Rejected
toResult rule = Next rule

execute :: WorkFlow -> Rating -> Result
execute = executeRule "in"

executeRule :: String -> WorkFlow -> Rating -> Result
executeRule rule workflow rating = executeConditions workflow rating $ conditions $ head $ filter (\r -> rulename r == rule) workflow

executeConditions :: WorkFlow -> Rating -> [Condition] -> Result
executeConditions [] = error "Out of conditions"
executeConditions (c : cs) = case executeCondition c of
                               Accepted -> Accepted
                               Rejected -> Rejected


main :: IO ()
main = do
  result <- parseFromFile parseInput "test.txt"

  case result of
    Left err -> print err
    Right xs -> print xs
