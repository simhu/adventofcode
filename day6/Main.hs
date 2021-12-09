{-# LANGUAGE LambdaCase #-}
module Main where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

newtype Pool = Pool { unPool :: [Int] }
  deriving (Show, Eq)

sea :: Parser [Int]
sea = num `sepBy` char ','
  where num = read <$> many1 digit

fromSea :: [Int] -> Pool
fromSea ns = Pool $ map (\n -> length (filter (==n) ns)) [0..8]

evolve :: Pool -> Pool
evolve (Pool fish) = Pool $ xs ++ ((nextGen+y):ys) ++ [nextGen]
  where nextGen    = head fish
        (xs, y:ys) = splitAt 6 (tail fish)

alive :: Pool -> Int
alive = sum . unPool

main :: IO ()
main = do
  Right s <- parseFromFile sea "input"
  putStrLn "PART 1"
  putStrLn (show (alive ((iterate evolve (fromSea s)) !! 80)))

  putStrLn "PART 2"
  putStrLn (show (alive ((iterate evolve (fromSea s)) !! 256)))


