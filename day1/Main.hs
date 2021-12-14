module Main where

--- Part 1
count :: (Ord a) => [a] -> Int
count [] = error "count: empty input"
count xs = length $ concat $
  zipWith (\x y -> [() | x < y]) xs (tail xs)

main :: IO ()
main = do
  -- Part 1:
  strs <- lines <$> readFile "input"
  let input :: [Int]
      input = map read strs
  putStrLn "PART1:"
  print (count input)
  -- Part 2:
  let sums = zipWith3 (\x y z -> x + y + z)
             input (tail input) (tail (tail input))
  putStrLn "PART2:"
  print (count sums)



