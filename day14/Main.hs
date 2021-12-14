module Main where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.List (partition, sort)

type Template = String
type Rules = [((Char, Char), Char)]
type Counts = Map (Char, Char) Int

rules :: Parser Rules
rules = ((,) <$> ((,) <$> letter <*> letter) <*> (string " -> " *> letter))
        `sepEndBy` endOfLine

countList :: Eq a => [a] -> [(a,Int)]
countList []     = []
countList (x:xs) = (x, length eqx + 1):countList neqx
 where (eqx,neqx) = partition (==x) xs

toCounts :: Template -> Counts
toCounts xs = M.fromList $ countList $ zip xs (tail xs ++ [' '])

step :: Rules -> Counts -> Counts
step rules cs = M.unionsWith (+) (cs:add)
  where add = [ M.fromListWith (+) [((l,r),-n), ((l,c),n), ((c,r), n) ]
              | ((l,r), c) <- rules, (l,r) `M.member` cs , let n = cs ! (l,r) ]

counts :: Counts -> [(Char,Int)]
counts = M.toList . M.mapKeysWith (+) fst

main :: IO ()
main = do
  let p = (,) <$> (many letter <* count 2 endOfLine) <*> rules
  Right (t,rs) <- parseFromFile p "input"
  let answ x = last ns - head ns
        where ns = sort $ map snd $ counts $ iterate (step rs) (toCounts t) !! x
  print $ answ 10
  print $ answ 40
