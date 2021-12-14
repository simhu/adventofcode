{-# LANGUAGE LambdaCase #-}
module Main where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Data.List ((\\))
import Data.Maybe (isJust,maybeToList)
import qualified Data.Map as M
import Data.Map ((!))

data Vertex = Start | End | Small String | Large String
  deriving (Eq,Ord)

instance Show Vertex where
  show Start     = "start"
  show End       = "end"
  show (Small s) = s
  show (Large s) = s

type Graph = M.Map Vertex [Vertex]

toGraph :: [(Vertex, Vertex)] -> Graph
toGraph [] = M.empty
toGraph ((f,t):es) = M.insertWith (++) f [t] $ M.insertWith (++) t [f] $ toGraph es

graph :: Parser Graph
graph = toGraph <$> edge `sepEndBy1` newline
  where vertex = string "start" *> return Start <|> string "end" *> return End <|>
          Small <$> many1 (oneOf ['a'..'z']) <|> Large <$> many1 (oneOf ['A'..'Z'])
        edge = (,) <$> vertex <*> (char '-' *> vertex)

isLarge :: Vertex -> Bool
isLarge (Large _) = True
isLarge _         = False

allPaths :: Graph -> Vertex -> Vertex -> [[Vertex]]
allPaths g s e = go [] [] s
  where
    go acc smallvisited v
      | v == e    = [reverse (e:acc)]
      | null suc  = []
      | isLarge v = concatMap (go (v:acc) smallvisited) suc
      | otherwise = concatMap (go (v:acc) (v:smallvisited)) suc
      where suc = g ! v \\ smallvisited

small :: [Vertex] -> [Vertex]
small = filter (\case { Small _ -> True; _ -> False })

allPaths2 :: Graph -> Vertex -> Vertex -> [[Vertex]]
allPaths2 g s e = concatMap (\v -> go [] [] v s) (Nothing:map Just (small $ M.keys g))
  where
    go acc smallvisited twice v
      | Just v' <- twice, length (filter (==v') smallvisited) > 2 = []
      | v == e    = case twice of
                      Just v' | length (filter (==v') smallvisited) < 2 -> []
                      _ -> [reverse (e:acc)]
      | null suc  = []
      | isLarge v = concatMap (go (v:acc) smallvisited twice) suc
      | otherwise = concatMap (go (v:acc) (v:smallvisited) twice) suc
      where suc = g ! v \\ (smallvisited \\ maybeToList twice)

main :: IO ()
main = do
  Right es <- parseFromFile graph "input"
  let ap = allPaths es Start End
  print (length ap)
  let ap2 = allPaths2 es Start End
  print (length ap2)

