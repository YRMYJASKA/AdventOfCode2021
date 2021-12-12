import System.IO()
import Text.Printf
import Data.List.Index
import Data.Array

-- Read solution and pass it on
main :: IO ()

type Grid = Array Int (Array Int (Int, Bool))


computeFlashes :: Grid -> Int -> [(Grid, Int)]
computeFlashes grid dim
  = scanl (\(g, _) _ -> let (g', f') = computeIter g in (g',f')) (grid, 0) (repeat 0)
  where
    flashAt :: Grid -> Int -> (Int, Int) -> (Grid, Int)
    flashAt g flashes pt@(y, x)
      | x >= dim || x < 0 || y >= dim || y < 0 = (g, flashes) -- Checking bounds
      | state                                  = (g, flashes)
      | num > 9                                = adjFlashes
      | num + 1 > 9                            = flashAt incGrid flashes pt
      | otherwise                              = (incGrid, flashes)
      where
        row'         = g ! y
        (num, state) = (row') ! x

        -- Add one to the current point
        incGrid      = g // [(y, row' // [(x, (num + 1, state))])]
        flipGrid     = g // [(y, row' // [(x, (0, True))])]

        --- Recursive steps:
        nextPts      = [(y + ix, x + iy) | ix <- [-1, 0, 1], iy <- [-1, 0, 1]]
        adjFlashes   = foldl (\(g', f') pt' -> flashAt g' f' pt') (flipGrid, flashes + 1) nextPts

    computeIter :: Grid -> (Grid, Int)
    computeIter g
      = (zeroedout, flashes)
      where
        pts = [(y, x) | x <- [0..(dim - 1)], y <- [0..(dim - 1)]]
        (flashed, flashes) = foldl (\(g', f) pt -> flashAt g' f pt ) (g, 0) pts
        zeroedout = flashed // [(y, (flashed ! y) // [(x, (fst ((flashed ! y) ! x), False)) | x <- [0..(dim -1 )] ] )
                               | y <- [0..(dim - 1)]]
    

main = do
    contentLines <- map (\x -> [c:[] | c <- x]) <$> lines <$> readFile "input.txt"

    -- Parsing Here
    let n       = length contentLines
        inputs' = map (map ((\x -> (x, False)) . read)) contentLines :: [[(Int, Bool)]]
        inputs'' = map (listArray (0, n - 1)) inputs'
        input    = listArray (0, n - 1) inputs''
        flashes  = computeFlashes input n
        part1    = sum $ map snd $ take 101 flashes
        part2    = head $ dropWhile (\(_, x) -> x /= n * n) $ indexed (map snd $ flashes)
        
    printf "Part 1: %d\n" part1
    printf "Part 2: %d\n" $ fst part2
