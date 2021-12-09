module Main where

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser, parseFromFile)
import Control.Monad.State

data Inst   = Fwd Int | Down Int | Up Int
  deriving (Eq, Show)
type Course = [Inst]
type Pos    = (Int, Int)

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

inst :: Parser Inst
inst  =  (string "forward" >> spaces >> Fwd <$> num)
     <|> (string "down" >> spaces >> Down <$> num)
     <|> (string "up" >> spaces >> Up <$> num)

course :: Parser Course
course = inst `sepEndBy` endOfLine

evalI :: Pos -> Inst -> Pos
evalI (x, y) (Fwd z)  = (x + z, y)
evalI (x, y) (Down z) = (x, y + z)
evalI (x, y) (Up z)   = (x, y - z)

eval :: Pos -> Course -> Pos
eval = foldl evalI

type Aim = Int

evalAI :: Pos -> Inst -> State Aim Pos
evalAI (x, y) (Fwd z) = do aim <- get
                           return (x + z, y + aim * z)
evalAI p (Down z)     = modify (+z) >> return p
evalAI p (Up z)       = modify (+(-z)) >> return p

evalA :: Pos -> Course -> State Aim Pos
evalA = foldM evalAI

main :: IO ()
main = do p <- parseFromFile course "input"
          case p of
            Left _ -> do putStrLn "parse failed"
                         return ()
            Right course -> do
              let final = eval (0,0) course
              putStrLn "PART 1"
              putStrLn ("Final position: " ++  show final)
              putStrLn ("Multiplied: " ++ show (fst final * snd final))
              putStrLn "PART 2"
              let finalA = evalState (evalA (0,0) course) 0
              putStrLn ("Final position: " ++  show finalA)
              putStrLn ("Multiplied: " ++ show (fst finalA * snd finalA))

