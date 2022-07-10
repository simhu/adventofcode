module Main where

import Control.Applicative ((<|>))
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (Parser, parseFromFile)
import Data.Maybe (catMaybes)

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show (Leaf n)     = show n
  show (Branch l r) = "[" ++ show l ++ "," ++ show r ++ "]"

type Snail = Tree Int

data Dir = L | R
  deriving (Show,Eq)
type Path = [Dir]

expPath :: Snail -> Maybe ((Int,Int), Path)
expPath x = go x 4
  where go (Branch (Leaf lv) (Leaf rv)) 0 = Just ((lv,rv),[])
        go x@Branch{}   0 = error $ "expPath: nesting " ++ show x
        go (Leaf _)     n = Nothing
        go (Branch l r) n = fmap (L:) <$> go l (n-1) <|> fmap (R:) <$> go r (n-1)

updLeft, updRight :: Snail -> Int -> Snail
updLeft  (Leaf n) m     = Leaf (n+m)
updLeft  (Branch l r) m = Branch (updLeft l m) r
updRight (Leaf n) m     = Leaf (n+m)
updRight (Branch l r) m = Branch l (updRight r m)

explode' :: Snail -> Path -> Int -> Int -> Snail
explode' x [] lv rv = Leaf 0
explode' (Branch l r) (L:p) lv rv = Branch l' r'
  where l' = explode' l p lv rv
        r' = if all (==R) p then updLeft r rv else r
explode' (Branch l r) (R:p) lv rv = Branch l' r'
  where r' = explode' r p lv rv
        l' = if all (==L) p then updRight l lv else l

explode :: Snail -> Maybe Snail
explode x = do ((lv,rv), p) <- expPath x
               return $ explode' x p lv rv

splitPath :: Snail -> Maybe Path
splitPath (Leaf n) | n >= 10 = Just []
splitPath (Leaf _)           = Nothing
splitPath (Branch l r)       = (L:) <$> splitPath l <|> (R:) <$> splitPath r

splitWithPath :: Snail -> Path -> Snail
splitWithPath (Leaf n) [] =
  Branch (Leaf $ n `div` 2) (Leaf $ n `div` 2 + n `mod` 2)
splitWithPath (Branch l r) (L:xs) = Branch (splitWithPath l xs) r
splitWithPath (Branch l r) (R:xs) = Branch l (splitWithPath r xs)

split :: Snail -> Maybe Snail
split x = splitWithPath x <$> splitPath x

iterMaybe :: (a -> Maybe a) -> a -> a
iterMaybe f x = case f x of
  Just y  -> iterMaybe f y
  Nothing -> x

nf :: Snail -> Snail
nf = iterMaybe (\x -> explode x <|> split x)

add :: Snail -> Snail -> Snail
add x y = nf (Branch x y)

magnitude :: Snail -> Int
magnitude (Leaf n)     = n
magnitude (Branch l r) = 3 * magnitude l + 2 * magnitude r

snail :: Parser Snail
snail =  Leaf <$> num
     <|> Branch <$> (char '[' *> snail) <*> (char ',' *> snail <* char ']')
  where num = read <$> many1 digit

maxMagnitudePair :: [Snail] -> Int
maxMagnitudePair []     = -1
maxMagnitudePair (x:xs) = maximum $ maxMagnitudePair xs:
    [ max (magnitude (add x y)) (magnitude (add y x)) | y <- xs]

main :: IO ()
main = do
  Right xs <- parseFromFile (snail `sepEndBy1` newline) "input"
  print (magnitude (foldl1 add xs))
  print (maxMagnitudePair xs)
