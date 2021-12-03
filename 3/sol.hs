import System.IO
import Text.Printf
import Data.Char

--- Small helper 
convert   = foldl1 (\a b -> 2*a + b)

--- Part 1 solution
part1 :: [[Int]] -> (Int, Int)
part1 input
  = (gamma, epsilon)
  where
    n         = length (input !! 0)
    threshold = floor ((fromIntegral $ length input) / 2) -- O(n) time...
    initVec   = [0 | _ <- [1..n]] -- 12 digits
    sums      = foldl (zipWith (+)) initVec input
    gamma'    = map (\x -> if x > threshold then 1 else 0) sums
    gamma     = convert gamma'
    epsilon   = convert $ zipWith (-) [1 | _ <- [1..n]] gamma'
    

--- Part 2 solution
--part2 :: [[Int]] -> (Int, Int)
part2 ls
  = (genNum oxyCrit 0 ls, genNum co2Crit 0 ls)
  where
    oxyCrit s t = if s >= t then 1 else 0
    co2Crit s t = if s >= t then 0 else 1

    genNum _ _ [a]
      = convert a
    genNum crit n xs
      = genNum crit (n+1) xs'
      --- = genNum crit (n + 1) xs'
      where
        thresh = ceiling $ ((fromIntegral . length) xs) / 2
        summa  = foldl (\a b -> a + (b !! n)) 0 xs
        toKeep = crit summa thresh
        xs'    = filter (\x -> (x !! n) == toKeep) xs

-- Read solution and pass it on
main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    -- Parsing Here
    let contentLines = lines contents
        inputs       = map (map ((-48 + ) . ord)) contentLines

    printf "Part 1: %v \n" $ uncurry (*) (part1 inputs)
    printf "Part 2: %v \n" $ uncurry (*) (part2 inputs)
