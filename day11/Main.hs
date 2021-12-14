module Main where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Data.Monoid
import Control.Monad.Writer
import Control.Monad (foldM)
import Data.List (delete, findIndex)
import qualified Data.Map.Strict as M

type Loc = (Int,Int)
type Grid = M.Map Loc Int

annotate :: [[a]] -> [(Loc,a)]
annotate zss = [ ((x,y),z) | (y,zs) <- zip [0..] zss, (x,z) <- zip [0..] zs ]

grid :: Parser Grid
grid = (M.fromList . annotate) <$>
       many (read . singleton <$> digit) `endBy` newline
  where singleton x = [x]

adj :: Loc -> [Loc]
adj (x,y) = delete (x,y) [ (x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1] ]

step :: Grid -> Writer (Sum Int) Grid
step g = go (M.map (+1) g) M.empty
  where go :: Grid -> Grid -> Writer (Sum Int) Grid
        go g flashed = let fl = M.filter (> 9) (M.difference g flashed) in
          if M.null fl then return $ M.unionWith (const (const 0)) g flashed
          else do
            let inc = M.foldMapWithKey (const . adj) fl
                g' = foldr (M.adjust (+1)) g inc
            tell (Sum $ M.size fl)
            go g' (flashed `M.union` fl)

main :: IO ()
main = do
  Right g <- parseFromFile grid "input"
  -- Part 1
  let it100 = foldM (const . step) g [0..99]
  putStrLn (show (getSum (execWriter it100)))
  -- Part 2
  let iter = iterate (fst . runWriter . step) g
  let Just n = findIndex (all ((==0) . snd) . M.assocs) iter
  putStrLn (show n)
