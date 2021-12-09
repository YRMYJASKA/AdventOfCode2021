import System.IO
import Text.Printf
import Data.Maybe
import Data.Array
import qualified Data.Map as M

--- Part 1 solution
part1 nums curr w
  | curr == 0               = topCase + part1 nums (curr+1) w
  | curr >= length nums     = 0
  | curr == length nums - 1 = bottomCase
  | otherwise               = genCase

--- Part 2 solution
part2 
  = undefined


-- Read solution and pass it on
main :: IO ()
main = do
    contentLines <- map (intersperse ',') <$> lines <$> readFile "example.txt"

    -- Parsing Here
    let inputs       = map (\x -> read ("[" ++ x ++ "]")) contentLines :: [[Int]]
    print inputs
    printf "Part 1: %v \n" (part1 inputs 0 (length $ head inputs) 0)
    printf "Part 2: %d \n" (part2 inputs)
