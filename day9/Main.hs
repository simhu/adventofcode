{-# LANGUAGE TupleSections #-}
module Main where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Data.List

type Grid = [[Int]]

grid :: Parser Grid
grid = many (read . singleton <$> digit) `endBy` newline
  where singleton x = [x]

getVLowIndicesAndVals :: [Int] -> [(Int, Int)]
getVLowIndicesAndVals xs =
  [ (i, x) | (i, x, l, r) <- zip4 [0..] xs lxs rxs , x < l && x < r ]
  where (lxs, rxs) = (10:init xs, tail xs ++ [10])

getLows :: Grid -> [Int]
getLows g = map (snd . snd) lows
  where idcs f = concat (map
                 (\(i,xs) -> map (i,) (getVLowIndicesAndVals xs))
                 (zip [0..] f))
        vlows = idcs g
        hlows' = idcs (transpose g)
        hlows = map (\(y,(x,v)) -> (x,(y,v))) hlows'
        lows = vlows `intersect` hlows

main :: IO ()
main = do
  Right g <- parseFromFile grid "input"
  putStrLn "PART 1"
  putStrLn (show (sum (map (+1) (getLows g))))

