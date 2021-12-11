module Main where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Data.List (sortBy)
import qualified Data.Map.Strict as M

type Loc = (Int,Int)
type Grid = M.Map Loc Int
type PGrid = M.Map Loc Int -- partial grid

annotate :: [[a]] -> [(Loc,a)]
annotate zss = [ ((x,y),z) | (y,zs) <- zip [0..] zss, (x,z) <- zip [0..] zs ]

grid :: Parser Grid
grid = (M.fromList . annotate) <$>
       many (read . singleton <$> digit) `endBy` newline
  where singleton x = [x]

getNhd :: Loc -> [Loc]
getNhd (x,y) = [(x,y-1),(x,y+1),(x-1,y),(x+1,y)]

(!!!) :: Grid -> Loc -> Int
(!!!) = flip $ M.findWithDefault 9

lowPoints :: Grid -> PGrid
lowPoints g = M.filterWithKey (\l v -> all (v <) (map (g !!!) (getNhd l))) g

connects :: Grid -> PGrid -> PGrid
connects g marked = marked `M.union` M.fromList new
  where mark l val = let nh = getNhd l in
          filter (\(_,v) -> v < 9 && val < v) (zip nh $ map (g !!!) nh)
        new = M.foldMapWithKey mark (M.intersection g marked)

-- The basin associated to a low point.
growBasin :: Grid -> Loc -> Int -> PGrid
growBasin g l v = growBasin' g (M.singleton l v)
  where
    growBasin' g m = let m' = connects g m in
      if M.keys m == M.keys m' then m else growBasin' g m'

basins :: Grid -> [PGrid]
basins g = M.foldMapWithKey (\l v -> [growBasin g l v]) (lowPoints g)

main :: IO ()
main = do
  Right g <- parseFromFile grid "input"
  putStrLn "PART 1"
  let lp = M.elems (lowPoints g)
  putStrLn (show (length lp + sum lp))
  putStrLn "PART 2"
  let bs  = basins g
      bs' = sortBy (flip compare) (map (length . M.keys) bs)
  putStrLn (show (product (take 3 bs')))
