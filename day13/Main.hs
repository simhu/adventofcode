module Main where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import qualified Data.Set as S

type Transparent = S.Set (Int,Int)
data Fold = X Int | Y Int
  deriving (Eq, Show)

num :: Parser Int
num = read <$> many1 digit

trans :: Parser Transparent
trans = fromlist <$> (loc `endBy1` newline)
  where loc = (,) <$> (num <* char ',') <*> num
        fromlist = foldr S.insert S.empty

folds :: Parser [Fold]
folds = (string "fold along " *>
         (X <$> (string "x=" *> num) <|> Y <$> (string "y=" *> num)))
        `sepEndBy1` newline

fold :: Transparent -> Fold -> Transparent
fold m (X x) = -- NB: assumption colum x is empty in m
  let (left,right') = (S.partition (\(i,_) -> i < x) m)
      right = S.map (\(i,j) -> (2 * x - i, j)) right'
  in S.union left right
fold m (Y y) = swap (fold (swap m) (X y))
  where swap = S.map (\(i,j) -> (j,i))

showTransparent :: Transparent -> String
showTransparent t =
  let (maxx, maxy) = (maximum $ S.map fst t, maximum $ S.map snd t)
  in unlines
     [[if (x,y) `S.member` t then '#' else ' ' | x <- [0..maxx]] | y <- [0..maxy]]

main :: IO ()
main = do
  Right (t,fs) <- parseFromFile ((,) <$> trans <*> (newline *> folds)) "input"
  let f1 = head fs
  putStrLn (show $ length (S.elems $ fold t f1))
  putStrLn (showTransparent $ foldl fold t fs)
