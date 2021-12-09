import System.IO
import Text.Printf
import Data.Maybe
import Data.List.Index (indexed)
import Data.Sort
import qualified Data.Set as S
import qualified Data.Map as M

--- Part 1 solution
part1 []
  = 0
part1 ((_, outputs):rest) 
  = amount + part1 rest
  where
    amount = length $ filter (\x -> length x `elem` [2, 4, 3, 7]) outputs
    
--- Part 2 solution
properMapping = map S.fromList ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg","acf",  "abcdefg", "abcdfg"]

part2 []
  = 0
part2 ((signals, outputs):rest)
  = (digits !! 0) * 1000 +  (digits !! 1) * 100 + (digits !! 2) * 10 + (digits !! 3) + part2 rest
  where
    startGrid = M.fromList [(x, S.fromList "") | x <- "abcdefg"]
    -- Special numbers
    addNum grid scramble numStr 
      = foldl (\g c -> M.insertWith S.union c scramble g) grid numStr
    isIn g
      = M.foldl S.union (S.fromList "") g
    -- Find scrambles
    one    = S.fromList $ head $ filter (\x -> length x == 2) signals
    four   = S.fromList $ head $ filter (\x -> length x == 4) signals
    seven  = S.fromList $ head $ filter (\x -> length x == 3) signals
    eight  = S.fromList $ head $ filter (\x -> length x == 7) signals
    grid1 = addNum startGrid one (S.fromList "cf")
    grid2 = addNum grid1 (S.difference four (isIn grid1)) (S.fromList "bd")
    grid3 = addNum grid2 (S.difference seven (isIn grid2)) (S.fromList "a")
    grid4 = addNum grid3 (S.difference eight (isIn grid3)) (S.fromList "eg")

    fivetwothree = map S.fromList ( filter (\x -> length x == 5) signals)
    zerosixnine  = map S.fromList ( filter (\x -> length x == 6) signals)
    d' = foldl (\b a -> S.intersection b a) (S.fromList "abcdefg") fivetwothree
    d = S.difference(S.difference d' (grid4 M.! 'a')) (grid4 M.! 'g')
    grid4' = M.insert 'd' d grid4
    grid5  = M.insertWith (flip S.difference) 'b' (d) grid4'

    f'  = foldl (\b a -> S.intersection b a) (S.fromList "abcdefg") zerosixnine
    f = foldl (S.difference) f' [grid5 M.! c | c <- "abge"]
    grid5' = M.insert 'f' (S.take 1 f) grid5
    grid6 = M.insertWith (flip S.difference) 'c' (f) grid5'

    g = S.difference(S.difference d' (grid6 M.! 'a')) (grid6 M.! 'd')
    grid6' = M.insert 'g' g grid6
    grid7  = M.insertWith (flip S.difference) 'e' (g) grid6'
    convert rep = S.unions inter
      where
        inter = S.map (\x -> grid7 M.! x) rep
    numMap = [(convert r, i) | (i, r) <- indexed properMapping]

    digits = map (\x -> fromJust (lookup x numMap)) (map (S.fromList) outputs)


-- Read solution and pass it on
main :: IO ()
main = do
    contentLines <- lines <$> readFile "input.txt"

    -- Parsing Here
    let inputs'      = map (splitAt 10 . words) contentLines
        inputs       = map (\(a,b) -> (a, tail b)) inputs'

    printf "Part 1: %v \n" (part1 inputs)
    print (part2 inputs)
