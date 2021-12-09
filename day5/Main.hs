{-# LANGUAGE LambdaCase #-}
module Main where

import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Data.Map.Strict as M

data Edge a = Edge a a
  deriving (Eq, Show)

type Point = (Int, Int)
type Line = Edge Point

line :: Parser Line
line = Edge <$> point <*> (string " -> " >> point)
  where point = (,) <$> num <*> (char ',' >> num)
        num = read <$> many1 digit

isHorV :: Line -> Bool
isHorV (Edge (x1,y1) (x2,y2)) = x1 == x2 || y1 == y2

region :: (Ord a, Enum a) => a -> a -> [a]
region x y | x <= y    = [x..y]
region x y | otherwise = reverse [y..x]

-- like zip, but shorter list gets padded with its last entry
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

extCount :: [Point] -> M.Map Point N2
extCount []     = M.empty
extCount (p:ps) = M.insertWith (\_ _ -> TwoOrMore) p One (extCount ps)

countCritOverlaps :: [Point] -> Int
countCritOverlaps =
  length . filter (==TwoOrMore) . M.elems . extCount

main :: IO ()
main = do
  Right ls <- parseFromFile (many (line <* endOfLine)) "input"
  let hvpts = concatMap lineToPts (filter isHorV ls)
  putStrLn "PART 1"
  putStrLn ("Two or more overlaps at " ++
            show (countCritOverlaps hvpts) ++ " points")
  putStrLn "PART 2"
  let hvdpts = concatMap lineToPts ls
  putStrLn ("Two or more overlaps at " ++
            show (countCritOverlaps hvdpts) ++ " points")
