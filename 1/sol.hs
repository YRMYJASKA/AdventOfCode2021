import System.IO
import Text.Printf

--- Part 1 & Part 2 solution
generalSol :: Int -> [Int] -> Int
generalSol n lst
  | length lst <= n = 0
  | otherwise       = indicator + generalSol n (tail lst)
  where
    diff = (lst !! n) - (lst !! 0)
    indicator = if diff > 0 then 1 else 0

-- Read input and pass it on
main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    -- Parsing
    let contentLines = lines contents
        inputs       = map read contentLines

    printf "Part 1: %v \n" (generalSol 1 inputs)
    printf "Part 2: %v \n" (generalSol 3 inputs)
