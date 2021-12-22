{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.State
import Control.Monad (replicateM)
import Data.Maybe (fromJust)

data Packet = Lit Version Int
            | Op Version Op [Packet]
  deriving (Eq, Show)

data Op = Sum | Prod | Min | Max | Greater | Less | Equal
  deriving (Eq, Show)

type Version = Int
type Parser  = State [Bool]

-- TODO: clean up

hexToBits :: Int -> [Bool]
hexToBits n = reverse (go n 4)
  where go _ 0 = []
        go n i = (n `rem` 2 == 1):go (n `div` 2) (i-1)

bitsToInt :: [Bool] -> Int
bitsToInt bs = go 0 bs
  where go n [] = n
        go n (b:bs) = go (2*n + if b then 1 else 0) bs


parse :: String -> [Bool]
parse = concatMap (\c -> hexToBits (read ("0x" ++ [c])))

pull :: Int -> Parser [Bool]
pull n = do s <- get
            let (a,s') = splitAt n s
            put s'
            return a

first :: Parser Bool
first = do xs <- pull 1
           return (head xs)

id4 :: Parser Int
id4 = bitsToInt <$> go
  where go :: Parser [Bool]
        go = do x <- first
                xs <- pull 4
                if x then (xs ++) <$> go
                  else return xs

consumeWith :: Int -> Parser a -> Parser [a]
consumeWith n p | n < 0 = error "consumeWith: negative"
consumeWith 0 p = return []
consumeWith n p = do
  m <- gets length
  a <- p
  k <- gets length
  (a:) <$> consumeWith (n - (m - k)) p

packet :: Parser Packet
packet = do
  ver <- bitsToInt <$> pull 3
  id  <- bitsToInt <$> pull 3
  case id of
    4 -> Lit ver <$>id4
    n -> Op ver (toOp n) <$> opArgs

toOp :: Int -> Op
toOp = \case {0 -> Sum; 1 -> Prod ; 2 -> Min ; 3 -> Max
             ; 5 -> Greater ; 6 -> Less ; 7 -> Equal}

opArgs :: Parser [Packet]
opArgs = first >>= \case
  True -> do nSubPackets <- bitsToInt <$> pull 11
             replicateM nSubPackets packet
  _    -> do len <- bitsToInt <$> pull 15
             consumeWith len packet

toPacket :: [Bool] -> Maybe Packet
toPacket bs = let (p, rest) = runState packet bs in
  if or rest then Nothing else Just p

versionSum :: Packet -> Int
versionSum (Lit v _) = v
versionSum (Op v _ ps) = v + sum (map versionSum ps)

eval :: Packet -> Int
eval (Lit _ n) = n
eval (Op _ op ps) = case op of
  Sum     -> sum vs
  Prod    -> product vs
  Min     -> minimum vs
  Max     -> maximum vs
  Greater -> if vs !! 0 >  vs !! 1 then 1 else 0
  Less    -> if vs !! 0 <  vs !! 1 then 1 else 0
  Equal   -> if vs !! 0 == vs !! 1 then 1 else 0
  where vs = map eval ps

main :: IO ()
main = do
  s:_ <- lines <$> readFile "input"
  let allbits = parse s
  let Just p = toPacket allbits
  print (versionSum p)
  print (eval p)
