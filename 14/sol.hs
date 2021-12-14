{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Text.Printf
import Data.Maybe
import qualified Data.Map as M
import Data.Text (splitOn)
import Debug.Trace (trace)

adjPairs :: [a] -> [(a,a)]
adjPairs []
  = []
adjPairs [a]
  = []
adjPairs (x:y:rest)
  = (x,y) : adjPairs (y:rest)

countArr :: Ord a => [a] -> M.Map a Int
countArr []
  = M.empty
countArr (x:xs)
  = M.unionWith (+) (M.fromList [(x,1)]) (countArr xs)


polymerIter :: M.Map (Char, Char) Char -> (M.Map Char Int, M.Map (Char, Char) Int) -> (M.Map Char Int, M.Map (Char, Char) Int)
polymerIter rules (charCounts, pairCounts)
  = (newCharCounts, newPairCounts)
  where
    newCharCounts = M.unionWith (+) charCounts additionals
    additionals   = M.mapKeysWith (+) (rules M.!) pairCounts
    newPairCounts = M.foldlWithKey (\acc k count -> M.unionWith (+) acc (calcPairs k count)) M.empty pairCounts

    calcPairs :: (Char, Char) -> Int -> M.Map (Char, Char) Int
    calcPairs xy@(x,y) count
      = M.fromList [((x, m), count), ((m, y), count)]
      where
        m = rules M.! xy


generalSol p r n
  = most - least
  where
    most      = maximum finalVals
    least     = minimum finalVals
    finalVals = (map snd . M.toList . fst . head . reverse . take (n+1)) $ iterate (polymerIter r) (countArr p, countArr $ adjPairs p)

part1 p r = generalSol p r 10
part2 p r = generalSol p r 40

-- Read solution and pass it on
main :: IO ()
main = do
    (initialPolymer:_:rules') <- lines <$> readFile "input.txt"

    -- Parsing Here
    let rules = M.fromList $ map ((\(a:aa:[],b) -> ((a,aa), head $ drop 4 b)) . splitAt 2) rules' 
    printf "Part 1: %d \n" (part1 initialPolymer rules)
    printf "Part 2: %d \n" (part2 initialPolymer rules)
