import System.IO
import Text.Printf
import Debug.Trace
import Data.Maybe
import Data.Text (splitOn, pack, unpack)
import Data.Char (isUpper)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph as G

type Node = String
type Network = M.Map Node (S.Set Node)

--- Part 1 solution -> Int
generalSol :: G.Graph -> (G.Vertex -> (String, String, [String])) -> (String -> G.Vertex) -> Bool-> Int
generalSol g vertexToEntry keyToVertex repeat
  = pathSearch start S.empty repeat
  where
    end   = keyToVertex "end"
    start = keyToVertex "start"
    pathSearch :: G.Vertex -> S.Set G.Vertex -> Bool -> Int 
    pathSearch curr visited repeated 
      | curr == end                                 = 1
      | (curr == start) && start `S.member` visited = 0
      | repeated && currInVisited                   = 0
      | otherwise                                   = sum recurses
      where
        adj            = map keyToVertex (thrT (vertexToEntry curr))
        currInVisited  = curr `S.member` visited
        repeated'      = repeated || currInVisited
        visited'       = if upStr (fstT (vertexToEntry curr)) then visited else S.insert curr visited
        recurses       = map (\next -> pathSearch next visited' repeated') adj


upStr :: String -> Bool
upStr = isUpper . head

fstT (a, _, _) = a
sndT (_, b, _) = b
thrT (_, _, c) = c


--- Parsing functions
lookupConnections :: Ord a => a -> [(a, a)] -> S.Set a
lookupConnections k vals
  = S.unions [S.fromList fsts, S.fromList snds]
  where
    fsts = map snd $ filter (\x -> fst x == k) vals
    snds = map fst $ filter (\x -> snd x == k) vals

lookupNodes ::  Ord a => [(a, a)] -> S.Set a
lookupNodes vals
  = foldl (\b a -> S.unions [b, S.singleton (fst a), S.singleton (snd a)]) S.empty vals

--- Part 1 and 2 solutions
part1 g v k = generalSol g v k True
part2 g v k = generalSol g v k False

-- Read solution and print results
main :: IO ()
main = do
    contentLines <- map ((map unpack) . splitOn (pack "-") . pack)
      <$> lines <$> readFile "input.txt"
    let edges'  = map (\a -> (a !! 0, a !! 1)) contentLines
        edges   = [(x, x, S.toList (lookupConnections x edges')) | x <- S.toList (lookupNodes edges')]
        (network, vToE, kToV) = G.graphFromEdges (edges)
    printf "Part 1: %d \n" (part1 network vToE (fromJust . kToV))
    printf "Part 2: %d \n" (part2 network vToE (fromJust . kToV))

