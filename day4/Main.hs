{-# LANGUAGE LambdaCase #-}
module Main where

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser, parseFromFile)
import Data.List (findIndex, find, transpose)
import Data.Maybe (mapMaybe, listToMaybe)

-- TODO: cleanup?

data Cell = Marked Int | Unmarked Int
  deriving (Eq, Show)

type Board = [[Cell]]
type Move = Int

num :: Parser Int
num = read <$> many1 digit

cell :: Parser Cell
cell = Unmarked <$> num

board :: Parser Board
-- NB: Can't use 'spaces' instead of the 'many (char ' ')' as '\n'
-- also parsed by spaces..
board = many1 (many (char ' ') *> cell) `endBy1` endOfLine

isMarked :: Cell -> Bool
isMarked (Marked _) = True
isMarked _          = False

isWinner :: Board -> Bool
isWinner b = any (all isMarked) b || any (all isMarked) (transpose b)

mark :: Move -> Board -> Board
mark m = map (map (\case { Unmarked x | x == m -> Marked x; z -> z }))

getMarked :: [[Board]] -> Maybe (Int, Board)
getMarked [] = Nothing
getMarked (bs:bss)
 | Just b <- find isWinner bs = return (0, b)
 | otherwise                  = do (i,b) <- getMarked bss
                                   return (i+1,b)

getLooser :: [[Board]] -> Maybe (Int, Board)
getLooser bss =
  do i <- findIndex (\bs -> length (filter (not . isWinner) bs) == 1) bss
     let bs = bss !! i
     j <- findIndex (not . isWinner) bs
     let bssj = map (!! j) bss
     k <- findIndex isWinner bssj
     return (k, bssj !! k)

main :: IO ()
main = do
  let fparser = (,) <$> (num `sepBy1` char ',') <* endOfLine <* endOfLine
                    <*> board `sepEndBy1` endOfLine
  parseFromFile fparser "input" >>= \case
    Left err -> print err
    Right (moves, boards) -> do
      let bss = scanl (flip $ map . mark) boards moves
      putStrLn "PART 1"
      case getMarked bss of
        Nothing -> error "No game won!"
        Just (i,b)  -> do
          let last = moves !! (i - 1)  -- the scanl also contains boards (no move)
              usum = sum (concatMap (map (\case { Unmarked n -> n; _ -> 0 })) b)
          putStrLn ("Last move: " ++ show last)
          putStrLn ("Sum of unmarked elements on winner board: " ++ show usum)
          putStrLn ("Product: " ++ show (last * usum))
      putStrLn "PART 2"
      case getLooser bss of
        Nothing -> error "No clear looser!"
        Just (k,b)  -> do
          let last = moves !! (k - 1)
              usum = sum (concatMap (map (\case { Unmarked n -> n; _ -> 0 })) b)
          putStrLn ("Last looser move: " ++ show last)
          putStrLn ("Sum of unmarked elements on looser board: " ++ show usum)
          putStrLn ("Product: " ++ show (last * usum))


