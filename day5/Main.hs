{-# LANGUAGE LambdaCase #-}
module Main where

import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser, parseFromFile)
import Data.List (nub)
import qualified Data.Map.Strict as M

data Edge a = Edge {from :: a, to :: a}
  deriving (Eq, Show)

type Point = (Int, Int)
type Line = Edge Point

num :: Parser Int
num = read <$> many1 digit

point :: Parser Point
point = (,) <$> num <*> (char ',' >> num)

line :: Parser Line
line = Edge <$> point <*> (string " -> " >> point)

isHorV :: Line -> Bool
isHorV (Edge (x1,y1) (x2,y2)) = x1 == x2 || y1 == y2

region :: (Ord a, Enum a) => a -> a -> [a]
region x y | x <= y    = [x..y]
region x y | otherwise = reverse [y..x]

-- like zip, but shorter lists gets padded with its last entry
zip' :: [a] -> [b] -> [(a,b)]
zip' []     []     = []
zip' [x]    [y]    = [(x,y)]
zip' [x]    (y:ys) = (x,y):zip' [x] ys
zip' (x:xs) [y]    = (x,y):zip' xs [y]
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- Assumes line is horizontal, vertical, or diagonal.
lineToPts :: Line -> [Point]
lineToPts (Edge (x1,y1) (x2,y2)) = zip' (region x1 x2) (region y1 y2)

-- Let's count in a funny way..
data N2 = Zero | One | TwoOrMore
  deriving (Eq,Show)

suc :: N2 -> N2
suc Zero = One
suc One  = TwoOrMore
suc _    = TwoOrMore

extCount :: [Point] -> M.Map Point N2
extCount [] = M.empty
extCount (p:ps) = M.insertWith (\_ _ -> TwoOrMore) p One (extCount ps)

countCritOverlap :: [Point] -> Int
countCritOverlap =
  length . filter (==TwoOrMore) . M.elems . extCount

main :: IO ()
main = do
  Right ls <- parseFromFile (many (line <* endOfLine)) "input"
  let hvls  = filter isHorV ls
      hvpts = concatMap lineToPts hvls
  putStrLn "PART 1"
  putStrLn ("Two or more overlaps at " ++
            show (countCritOverlap hvpts) ++ " points")
  putStrLn "PART 2"
  let hvdpts = concatMap lineToPts ls
  putStrLn ("Two or more overlaps at " ++
            show (countCritOverlap hvdpts) ++ " points")
