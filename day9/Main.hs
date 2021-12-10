{-# LANGUAGE TupleSections #-}
module Main where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Data.List

type Grid = [[Int]]
type Nhd a = (a,a,a,a)
type Loc = (Int,Int)

grid :: Parser Grid
grid = many (read . singleton <$> digit) `endBy` newline
  where singleton x = [x]

getNhds :: a -> [[a]] -> [[(a,Nhd a)]]
getNhds def xss = map row (zip3 xss xssu xssd)
  where (xssu,xssd) = (repeat def:init xss, tail xss ++ [repeat def])
        row (xs,xsu,xsd) =
          zip xs (zip4 xsu xsd (def:init xs) (tail xs ++ [def]))

isLow :: Ord a => a -> Nhd a -> Bool
isLow x (u,d,l,r) = all (x <) [u,d,l,r]

getLowVals :: Grid -> [Int]
getLowVals =
  map fst . concat . map (filter (uncurry isLow)) . getNhds 10

annotate :: [[a]] -> [[(Loc,a)]]
annotate g = [[ ((x,y),c) | (x,c) <- zip [0..] cs ] | (y,cs) <- zip [0..] g ]

-- State [[Loc]]

-- type B = State [[Loc]]

-- [[Loc]]

-- connectsBasin :: Ord a => (Loc,a) -> Nhd (Loc,a) -> B Bool
-- connectsBasin (loc,x) (u,d,l,r) =
--   s <- get
--   any (\(loc',y) -> x < y && loc') [u,d,l,r]


-- :: (a -> Nhd a -> b) -> [[(a,Nhd a)]] -> [[b]]
-- map (map (uncurry f))

main :: IO ()
main = do
  Right g <- parseFromFile grid "input"
  putStrLn "PART 1"
  putStrLn (show (sum (map (+1) (getLowVals g))))
  putStrLn "PART 2"
