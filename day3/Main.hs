module Main where

import Data.Function
import Data.List (nub, transpose, maximumBy, nubBy)

-- transpose :: [[a]] -> [[a]]
-- transpose [] = []
-- transpose ([]:xss) = []
-- transpose zss = map head zss : transpose (map tail zss)

toInt :: [Bool] -> Int
toInt xs = toInt' (reverse xs)
  where toInt' [] = 0
        toInt' (False:xs) = 2 * toInt' xs
        toInt' (True:xs) = 2 * toInt' xs + 1

-- defaults to true if both are equally often

sig' :: (Int -> Int -> Bool) -> [Bool] -> Bool
sig' f bs = f (length (filter (==True) bs)) (length (filter (==False) bs))

sig :: [Bool] -> Bool
sig = sig' (>=)

sigs :: [[Bool]] -> [Bool]
sigs = map sig

-- -- more general, one could do something like:
-- counts :: (Eq a) => [a] -> [(Int, a)]
-- counts xs = [ (length (filter (==x) xs), x) | x <- nub xs ]

-- sig :: Eq a => [a] -> a
-- sig = snd . maximumBy (compare `on` fst) . counts

-- sigs :: Eq a => [[a]] -> [a]
-- sigs = map sig

-- works on untransposed list
rate :: (Int -> Int -> Bool) -> [[Bool]] -> [Bool]
rate f []   = error "rate: empty"
rate f [xs] = xs
rate f xss  = s : rate f (map tail $ filter (\(x:_) -> x == s) xss)
  where
    s = sig' f (map head xss)

oxyRate :: [[Bool]] -> [Bool]
oxyRate = rate (>=)

co2Rate :: [[Bool]] -> [Bool]
co2Rate = rate (<)

main :: IO ()
main = do
  input <- map (map (=='1')) . lines <$> readFile "input"
  let bgamma = sigs (transpose input)
      (gamma, eps) = (toInt bgamma, toInt (map not bgamma))
  putStrLn "PART 1"
  putStrLn ("Gamma: " ++ show gamma ++ " Epsilon: " ++ show eps)
  putStrLn ("Product: " ++ show (gamma * eps))
  putStrLn "PART 2"
  let boxy = oxyRate input
      oxy  = toInt boxy
      bco2 = co2Rate input
      co2  = toInt bco2
  putStrLn ("Oxygen: " ++ show boxy ++ "\ndecimal: " ++ show oxy)
  putStrLn ("CO2:    " ++ show bco2 ++ "\ndecimal: " ++ show co2)
  putStrLn ("Product: " ++ show (oxy * co2))
