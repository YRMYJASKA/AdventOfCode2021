import System.IO
import Text.Printf
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Control.Exception as E

--- Part 1 solution
checkWin :: [[(Int, Bool)]] -> Bool
checkWin b
  = checkCols || checkRows 
  where
    cols      = [map (!! n) b | n <- [0..(length b -1)]]
    rows      = b
    checkCols = foldl1 (||) $ map (foldl (\p (_, t) -> p&&t) True) cols
    checkRows = foldl1 (||) $ map (foldl (\p (_, t) -> p&&t) True) rows

checkWinner :: [[[(Int, Bool)]]] -> Maybe Int
checkWinner []
  = Nothing
checkWinner (b:bs)
  | checkWin b = Just score
  | otherwise  = checkWinner bs
  where
    score = foldl1 (+) (map fst $ filter (not . snd) (concat b)) 

part1 :: [Int] -> [[[(Int, Bool)]]] -> Int
part1 [] _
  = error "No winners?"
part1 (new:rest) boards
  | winnerCheck /= Nothing = new * (fromJust winnerCheck)
  | otherwise              = part1 rest newBoards
  where
    newBoards   = map (map (map (\(a, b) -> (a, b || (a == new))))) boards
    winnerCheck = checkWinner newBoards
    

--- Part 2 solution
part2 :: [Int] -> [[[(Int, Bool)]]] -> Int
part2 drawn boards
  = head $winners [] drawn boards
  where
    winners lst [] _
      = lst
    winners lst _ []
      = lst
    winners lst (new:rest) boards'
      | winnerCheck /= Nothing = winners (new * score : lst) rest nextBoards
      | otherwise              = winners lst rest newBoards
      where
        newBoards   = map (map (map (\(a, b) -> (a, b || (a == new))))) boards'
        winnerCheck = checkWinner newBoards
        score       = fromJust winnerCheck 
        newLst      = (new * (fromJust winnerCheck)) : lst
        nextBoards  = filter (not . checkWin) newBoards
    


-- Read solution and pass it on
main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    -- Parsing Here
    let header       = (head . lines) contents
        drawnNumbers = map read (splitOn "," header) 
        boards'      = splitOn "\n\n" (drop (length header + 2) contents)
        boards''     = map lines boards'
        --- Abhorrent parsing:
        boards'''    = map (map (map  read . filter (/= "") . splitOn " ") ) boards'' :: [[[Int]]]
        boards       = map (map (map (\x -> (x, False)))) boards'''

    printf "Part 1: %v \n" (part1 drawnNumbers boards)
    printf "Part 2: %d \n" (part2 drawnNumbers boards)
