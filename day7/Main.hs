module Main where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

crabs :: Parser [Int]
crabs = (read <$> many1 digit) `sepBy` char ','

dPos :: Int -> Int -> Int
dPos c n = abs (c - n)

d :: [Int] -> Int -> Int
d cs n = sum (map (dPos n) cs)

d' :: [Int] -> Int -> Int
d' cs n = sum (map dPos' cs)
  where dPos' c = (dPos c n * (dPos c n + 1)) `div` 2

main :: IO ()
main = do
  Right cs <- parseFromFile crabs "input"
  let (lo, hi) = (minimum cs, maximum cs)
  let dd = map (d cs) [lo..hi]
  putStrLn "PART 1"
  putStrLn (show (minimum dd))
  putStrLn "PART 2"
  let dd' = map (d' cs) [lo..hi]
  putStrLn (show (minimum dd'))


