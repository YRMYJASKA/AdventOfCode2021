import System.IO
import Text.Printf
import Data.Maybe
import Data.Text (pack, unpack, splitOn)
import qualified Data.Map as M

--- Part 1 solution
---part1 :: [Int] -> Int
part1 xs costFunc
  = calcCost [mi..ma] [maxBound :: Int]
  where
    mi = minimum xs
    ma = maximum xs
    calcCost [] _
      = error "ran out of numbers?"
    calcCost (n : nn) costs@(prev:_)
      | cost > prev = prev
      | otherwise   = calcCost nn (cost:costs)
      where
        cost = foldl (\b a -> b + costFunc (abs (a-n))) (0) xs 
    

--- Part 2 solution
part2 inputs 
  = part1 inputs(\x -> x*(x+1) `div` 2) 


-- Read solution and pass it on
main :: IO ()
main = do
    contentLine <- (\x -> "[" ++ x ++ "]")<$> head <$> lines <$> readFile "input.txt"

    -- Parsing Here
    let inputs = read contentLine :: [Int]

    printf "Part 1: %v \n" (part1 inputs id)
    printf "Part 2: %d \n" (part2 inputs)
