import System.IO
import Text.Printf

--- Part 1 solution
part1 
  = undefined

--- Part 2 solution
part2 
  = undefined


-- Read solution and pass it on
main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    -- Parsing Here
    let contentLines = lines contents
        inputs       = undefined

    printf "Part 1: %v \n" (part1 inputs)
    printf "Part 2: %d \n" (part2 inputs)
