import System.IO()
import Text.Printf
import qualified Data.Set as S

data FoldLine = X | Y deriving (Show, Eq)
type Point = (Int, Int)
type Fold = (FoldLine, Int)

printBoard :: S.Set Point -> IO ()
printBoard pts = do
  mapM_ putStrLn grid
  where
    maxx     = maximum $ S.map fst pts
    maxy     = maximum $ S.map snd pts
    grid     = [[chooser (x, y) | x <- [0..maxx]] | y <- [0..maxy]]
    chooser pt
      | pt `S.member` pts = '#'
      | otherwise         = ' '
  

executeFold :: S.Set Point -> Fold -> S.Set Point
executeFold pts (coord, line)
  = newpts
  where
    newpts = S.foldl (\s pt -> S.insert (translate pt) s) S.empty pts
    translate (x, y)
      | coord == X && x > line = (2*line - x, y)
      | coord == Y && y > line = (x, 2*line - y)
      | otherwise              = (x, y)
    

--- Part 1 & 2 solutions
part1 pts fold
  = S.size $ executeFold pts fold

part2 pts folds 
  = foldl executeFold pts folds

-- Main 
main :: IO ()
main = do
    rawContent <- lines <$> readFile "input.txt"
    -- Parsing Here
    let pointLines = takeWhile (\x -> x /= []) rawContent
        points     = S.fromList $ map (read . (\x -> "(" ++ x ++ ")")) pointLines :: S.Set Point
        foldLines  = takeWhile  (\x -> x /= []) (reverse rawContent)
        folds'     = map (splitAt 1 . snd . splitAt 11) foldLines
        lineChooser "x" = X
        lineChooser "y" = Y
        folds      = reverse $ map (\(l, x:xs) -> (lineChooser l, read xs)) folds' :: [Fold]
    printf "Part 1: %d\n" $ part1 points (head folds)
    printf "Part2: \n"
    printBoard (part2 points folds)
