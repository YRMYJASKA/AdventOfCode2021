import System.IO
import Data.List.Split (splitOn)
import Data.Maybe
import Text.Printf
import qualified Data.Map as Map 

type Point = (Int, Int)
data Line  = Line Point Point deriving (Show)

--- General solution
generalSol :: [Line] -> Map.Map Point Int -> Int
generalSol [] m
  = length $ Map.filter (>1) m 
generalSol ((Line a b):ll) m 
  = generalSol ll newm
  where
    (dx, dy)             = (fst b - fst a, snd b - snd a)
    (xstart, xend)       = (min 0 dx, max 0 dx)
    (ystart, yend)       = (min 0 dy, max 0 dy)
    ortho                = signum dx * signum dy
    
    listOfPoints         = case dx of
                             0 -> [(fst a, snd a + y) | y <- [ystart..yend]]
                             _ -> [(fst a + x , snd a + x * ortho) | x <- [xstart..xend]]
    changePoint Nothing  = Just 1
    changePoint (Just x) = Just (x + 1)
    newm                 = foldl (flip (Map.alter changePoint)) m listOfPoints

--- Solutions
part1 xs
  = generalSol xs' Map.empty
  where
    xs' = filter (\(Line a b) -> (fst a == fst b) || (snd a== snd b)) xs
part2 
  = (flip generalSol) Map.empty


-- Read solution and pass it on
main :: IO ()
main = do
    content <- lines <$> readFile "input.txt"
    -- Parsing Here
    let inputs'  = map (splitOn " -> ") content
        inputs'' = map (map (read . (\x -> "("++x++")"))) inputs'  :: [[(Int, Int)]]
        inputs   = map (\x -> Line (x!!0) (x!!1)) inputs''
    putStrLn $ "Part 1: " ++ show (part1 inputs)
    putStrLn $ "Part 2: " ++ show (part2 inputs)
