{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Text.Printf
import Data.Maybe
import Data.Text (splitOn, pack, unpack)
import Data.Array

--- Part 1
emptyState = listArray (0,8) [0 | _ <- [0..8]]
part1 :: Array Integer Integer -> Integer -> Integer
part1 state 0
  = (sum . elems) state
part1 state turn
  = part1 newState (turn - 1) 
  where
    newState = state // [(n-1, state ! n) | n <- [1..8]] // [(6, state ! 7 + state ! 0), (8, state ! 0)]


--- Part 2 solution
part2 = part1


-- Read solution and pass it on
main :: IO ()
main = do
    content <- pack <$> head <$> lines <$> readFile "input.txt"

    -- Parsing Here
    let inputs   = map read $ (map unpack . splitOn ",") content
        inputArr = foldl (\a x -> a // [(x, 1 + a ! x)]) emptyState inputs


    printf "Part 1: %v\n" (part1 inputArr 80)
    printf "Part 2: %v\n" (part2 inputArr 256)
    ---printf "For fun: %v\n" (part2 inputArr 256000)
