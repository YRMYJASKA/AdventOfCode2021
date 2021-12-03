import System.IO
import Text.Printf


--- Part 1 solution
part1 :: [(String, Int)] -> Int
part1 xs
  = uncurry (*) $ foldl stateChanger (0, 0) xs
  where
    stateChanger (d, f) (ins, amount)
      | ins == "up"   = (max (d-amount) 0, f)
      | ins == "down" = (d+amount, f)
      | otherwise     = (d, f+amount)

--- Part 2 solution
part2 :: [(String, Int)] -> Int
part2 xs
  = computeProd $ foldl stateChanger (0, 0, 0) xs
  where
    computeProd (_,b,c) = b*c
    stateChanger (a, d, f) (ins, amount)
      | ins == "down" = (a + amount,d, f)
      | ins == "up"   = (a - amount,d, f)
      | otherwise     = (a, d + a*amount, f + amount)

-- Read solution and pass it on
main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    -- Parsing Here
    let contentLines = map words $ lines contents
        instructs   =  map ((!! 0)) contentLines
        amounts      =  map (read . (!! 1)) contentLines :: [Int]
    printf "Part 1: %v \n" (part1 $ zip instructs amounts)
    printf "Part 2: %v \n" (part2 $ zip instructs amounts)
