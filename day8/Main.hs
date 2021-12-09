module Main where

import Text.Parsec
import Text.Parsec.Char (letter)
import Text.Parsec.String (Parser, parseFromFile)
import Data.List
import Data.Maybe (fromJust)

type Seg = [Char]     -- subset of ['a'..'g']
type Display = [Seg]  -- length 10
type Out = [Seg]      -- length 4
type Perm = [(Char, Char)]

maxSeg :: Seg
maxSeg = ['a'..'g']

co :: Seg -> Seg
co = (\\) maxSeg

dis :: Parser (Display, Out)
dis = (,) <$> (seg `endBy1` char ' ') <*> (string "| " >> (seg `sepBy1` char ' '))
  where seg = many1 (oneOf maxSeg)

(#) :: Int -> Display -> [Seg]
(#) n = filter ((==n) . length)

intersects :: [Seg] -> Seg
intersects = foldr intersect maxSeg

solve :: Display -> Perm
solve ds = zip [a,b,c,d,e,f,g] maxSeg
  where [cf] = 2 # ds
        [acf] = 3 # ds
        [a] = acf \\ cf
        cde = co (intersects (6 # ds))
        [c] = cf `intersect` cde
        [f] = delete c cf
        [bcdf] = 4 # ds
        bd = bcdf \\ cf
        adg = intersects (5 # ds)
        cbef = co adg
        [b] = bd `intersect` cbef
        [d] = delete b bd
        [e] = cde \\ [c,d]
        [g] = maxSeg \\ [a,b,c,d,e,f]

dig :: Seg -> Int
dig x = case elemIndex x all of
  Just i  -> i
  Nothing -> error "dig: unknown segment"
  where all = [ "abcefg", "cf", "acdeg", "acdfg" , "bcdf", "abdfg"
              , "abdefg", "acf", "abcdefg", "abcdfg"]

dec :: [Int] -> Int
dec [] = 0
dec (n:ns) = n + 10 * dec ns

display :: Perm -> Out -> Int
display sigma out = dec (reverse out')
  where act x = fromJust (x `lookup` sigma)
        out'  = map (dig . sort . map act) out

main :: IO ()
main = do
  Right dos <- parseFromFile (many (dis <* newline)) "input"
  let outs = map snd dos
  putStrLn "PART 1"
  let part1 = length (filter (\s -> length s `elem` [2,3,4,7]) (concat outs))
  putStrLn (show part1)
  putStrLn "PART 2"
  let part2 = sum $ map (\(d,o) -> display (solve d) o) dos
  putStrLn (show part2)
