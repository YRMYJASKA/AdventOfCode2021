import System.IO
import Text.Printf
import Data.Maybe
import Data.Either
import Data.List (sort)

data Delim = Round | Square | Curly | Angled
  deriving (Eq, Show)

--- Part 1 solution
determineEnd :: [Delim] -> String -> Either [Delim] Delim
determineEnd stack ""
  = Left stack
determineEnd stack (x:ss)
  | x `elem` ")]}>"  = if x' == head stack then determineEnd (tail stack) ss else Right x'
  | otherwise        = determineEnd (x':stack) ss
  where
    x' = case x of
           '(' -> Round
           ')' -> Round
           '[' -> Square
           ']' -> Square
           '{' -> Curly
           '}' -> Curly
           '<' -> Angled
           '>' -> Angled

part1 :: [String] -> Int
part1 inputs
  = sum $ map score $ rights $map (determineEnd []) inputs
  where
    score c
      = case c of
          Round  -> 3
          Square -> 57
          Curly  -> 1197
          Angled -> 25137

--- Part 2 solution
part2 :: [String] -> Int
part2 inputs 
  = middleScore
  where
    cappings = lefts $ map (determineEnd []) inputs
    calcPoints = foldl (\b a -> b*5 + (score a)) 0
    points = map calcPoints cappings
    middleScore = (sort points) !! (length points `div` 2)
    score c
      = case c of
          Round -> 1
          Square -> 2
          Curly -> 3
          Angled -> 4

-- Read solution and pass it on
main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    printf "Part 1: %d \n" (part1 inputs)
    printf "Part 2: %d \n" (part2 inputs)
