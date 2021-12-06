import System.IO
import Text.Printf
import Data.Maybe
import qualified Data.Map as M

--- Part 1 solution
part1 
  = undefined

--- Part 2 solution
part2 
  = undefined


-- Read solution and pass it on
main :: IO ()
main = do
    contentLines <- lines <$> readFile "input.txt"

    -- Parsing Here
    let inputs       = undefined

    printf "Part 1: %v \n" (part1 inputs)
    printf "Part 2: %d \n" (part2 inputs)
