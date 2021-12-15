module Main where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import Data.List (transpose, delete)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.PSQueue as P
import Data.PSQueue (PSQ, Binding(..))

type Vertex = (Int,Int)
type Weight = Int
type Graph  = Map Vertex [(Weight,Vertex)]

grid :: Parser [[Int]]
grid = many1 (read <$> count 1 digit) `sepEndBy` newline

fromGrid :: [[Int]] -> Graph
fromGrid nss = foldr (\(s,wt) -> M.unionWith (++) (M.singleton s [wt])) M.empty edges
  where ann = [ [ ((x,y),v) | (x, v) <- zip [0..] row ] | (y,row) <- zip [0..] nss ]
        rowEdges :: [(Vertex,Weight)] -> [(Vertex, (Weight, Vertex))]
        rowEdges xs = concat
          [ [(l,(w',r)),(r,(w,l))] | ((l,w),(r,w')) <- zip xs (tail xs) ]
        edges = concatMap rowEdges (ann ++ transpose ann)

-- Dijkstra's algorithm: find the shortest distance to all vertices.
-- Returns distances and predecessors.  (The latter is not really
-- needed here..)
dijkstra :: Graph -> Vertex -> (Map Vertex Int, Map Vertex Vertex)
dijkstra g s = go S.empty queue (M.singleton s 0) M.empty
  where
    vertices = M.keys g
    queue = P.fromList ((s :-> 0):map (:->maxBound) (delete s vertices))
    go v q d p | P.null q = (d, p)
    go v q d p = go (S.insert u v) q'' d' p'
      where
        Just (u :-> n, q') = P.minView q
        su  = filter ((`S.notMember` v) . snd) (g ! u)
        (du, dp) = unzip
          [ ((v, a), (v, u)) | (w,v) <- su, let a = w + n,
            maybe True (a<) (d !? v) ] -- Nothing stands for infinity..
        q'' = foldr (uncurry P.insert) q' du
        d'  = foldr (uncurry M.insert) d du
        p'  = foldr (uncurry M.insert) p dp

gridExplode :: Int -> [[Int]] -> [[Int]]
gridExplode n xss = transpose $ map rowE $ transpose $ map rowE xss
  where
    inc x = x `mod` 9 + 1
    add x y = iterate inc x !! y
    rowE xs = zipWith add (cycle xs) (concatMap (replicate $ length xs) [0..n])

main :: IO ()
main = do
  Right nss' <- parseFromFile grid "input"
  let nss = gridExplode 4 nss'
      g = fromGrid nss
      s = (0,0)
      (d,_) = dijkstra g s
      both f (x, y) = (f x, f y)
      e = both maximum $ unzip (M.keys g)
  print (d !? e)
