module Main where

import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Either (lefts, rights)

open :: String
open = "([{<"

matching :: [(Char, Char)]
matching = zip open ")]}>"

val :: Char -> Int
val ')' = 3
val ']' = 57
val '}' = 1197
val '>' = 25137

score :: String -> Int
score (')':xs) = 1 + 5 * score xs
score (']':xs) = 2 + 5 * score xs
score ('}':xs) = 3 + 5 * score xs
score ('>':xs) = 4 + 5 * score xs
score _ = 0

check :: String -> Either Int Int
check xs = go xs []
  where go (x:xs) ys     | x `elem` open =
                           go xs (fromJust (lookup x matching) : ys)
        go (x:xs) (y:ys) | x == y        = go xs ys

        go (x:xs) (y:ys) | otherwise     = Left (val x)               -- corrupt
        go []     ys                     = Right (score (reverse ys)) -- incomplete

main :: IO ()
main = do
  ls <- lines <$> readFile "input"
  let ls' = map check ls
  print (sum (lefts ls'))
  let median x = sort x !! (length x `div` 2)
  print (median (rights ls'))
