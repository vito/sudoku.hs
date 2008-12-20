import Control.Monad (replicateM)
import Data.List (intersect, intercalate, (\\))
import Data.Char (digitToInt)
import System.Environment (getArgs)
import TestPJ (allTests)

type Grid  = [[Maybe Int]]
type Box   = [[Maybe Int]]
type Tile  = Maybe Int
type Coord = (Int, Int)

pad :: Int -> a -> [a] -> [a]
pad n x xs | length xs >= n = xs
           | otherwise = xs ++ (take (n - length xs) (repeat x))

instances :: (Eq a) => a -> [a] -> Int
instances m [] = 0
instances m (x:xs) | m == x    = 1 + instances m xs
                   | otherwise = instances m xs

split :: Int -> [a] -> [[a]]
split n [] = []
split n xs = take n xs : split n (drop n xs)

uniques :: (Eq a) => [a] -> [a]
uniques xs = filter ((== 1) . (flip instances xs)) xs

nums :: [Tile]
nums = map Just [1..9]

coord :: Grid -> Coord -> Tile
coord g (x, y) = (g !! y) !! x

range :: [Int] -> [Int] -> [Coord]
range xs ys = ys >>= ((zip xs) . repeat)

coordinates :: [Coord]
coordinates = range [0..8] [0..8]

row :: Grid -> Int -> [Tile]
row g n = g !! n

rowCoords :: Int -> [Coord]
rowCoords n = (split 9 coordinates) !! n

col :: Grid -> Int -> [Tile]
col g n = map (!! n) g

colCoords :: Int -> [Coord]
colCoords n = map (!! n) (split 9 coordinates)

box :: Coord -> Grid -> Box
box (x, y) g | x `elem` [0..2] && y `elem` [0..2] = numRange [0..2] [0..2] g -- Box 1
             | x `elem` [3..5] && y `elem` [0..2] = numRange [3..5] [0..2] g -- Box 2
             | x `elem` [6..8] && y `elem` [0..2] = numRange [6..8] [0..2] g -- Box 3
             | x `elem` [0..2] && y `elem` [3..5] = numRange [0..2] [3..5] g -- Box 4
             | x `elem` [3..5] && y `elem` [3..5] = numRange [3..5] [3..5] g -- Box 5
             | x `elem` [6..8] && y `elem` [3..5] = numRange [6..8] [3..5] g -- Box 6
             | x `elem` [0..2] && y `elem` [6..8] = numRange [0..2] [6..8] g -- Box 7
             | x `elem` [3..5] && y `elem` [6..8] = numRange [3..5] [6..8] g -- Box 8
             | x `elem` [6..8] && y `elem` [6..8] = numRange [6..8] [6..8] g -- Box 9

boxCoords :: (Int, Int) -> [Coord]
boxCoords (x, y) | x `elem` [0..2] && y `elem` [0..2] = range [0..2] [0..2] -- Box 1
                 | x `elem` [3..5] && y `elem` [0..2] = range [3..5] [0..2] -- Box 2
                 | x `elem` [6..8] && y `elem` [0..2] = range [6..8] [0..2] -- Box 3
                 | x `elem` [0..2] && y `elem` [3..5] = range [0..2] [3..5] -- Box 4
                 | x `elem` [3..5] && y `elem` [3..5] = range [3..5] [3..5] -- Box 5
                 | x `elem` [6..8] && y `elem` [3..5] = range [6..8] [3..5] -- Box 6
                 | x `elem` [0..2] && y `elem` [6..8] = range [0..2] [6..8] -- Box 7
                 | x `elem` [3..5] && y `elem` [6..8] = range [3..5] [6..8] -- Box 8
                 | x `elem` [6..8] && y `elem` [6..8] = range [6..8] [6..8] -- Box 9

set :: Grid -> Coord -> Tile -> Grid
set g c v = split 9 $ map (\(x, y) -> if (x, y) == c
                                      then v
                                      else coord g (x, y)) coordinates

possibilities :: Grid -> Coord -> [Tile]
possibilities g (x, y) | coord g (x, y) /= Nothing = []
                       | otherwise                 = (hPossibilities y g) `intersect`
                                                     (vPossibilities x g) `intersect`
                                                     (boxPossibilities (box (x, y) g))

hPossibilities :: Int -> Grid -> [Tile]
hPossibilities x g = filter (not . (`elem` (g !! x))) nums

vPossibilities :: Int -> Grid -> [Tile]
vPossibilities y g = filter (not . (`elem` (map (!! y) g))) nums

boxPossibilities :: Box -> [Tile]
boxPossibilities b = filter (not . (`elem` (concat b))) nums

numRange :: [Int] -> [Int] -> Grid -> Grid
numRange xs ys g = map (flip map xs . (!!)) (map (g !!) ys)

done :: Grid -> Bool
done g = and $ map (not . (Nothing `elem`)) g

valid :: Grid -> Bool
valid g = valid_rows && valid_cols && valid_boxes
          where
              valid_rows = and $ map (null . (\\ nums)) g
              valid_cols = and $ map (null . (\\ nums)) (map (col g) [0..8])
              valid_boxes = (and $ map (null . (\\ nums)) (box (0, 0) g)) && -- Box 1
                            (and $ map (null . (\\ nums)) (box (3, 0) g)) && -- Box 2
                            (and $ map (null . (\\ nums)) (box (6, 0) g)) && -- Box 3
                            (and $ map (null . (\\ nums)) (box (0, 3) g)) && -- Box 4
                            (and $ map (null . (\\ nums)) (box (3, 3) g)) && -- Box 5
                            (and $ map (null . (\\ nums)) (box (6, 3) g)) && -- Box 6
                            (and $ map (null . (\\ nums)) (box (0, 6) g)) && -- Box 7
                            (and $ map (null . (\\ nums)) (box (3, 6) g)) && -- Box 8
                            (and $ map (null . (\\ nums)) (box (6, 6) g))    -- Box 9

solve :: Grid -> Coord -> Tile
solve g (x, y) | solved      = coord g (x, y)
               | onePossible = head coordChoices
               | oneBox      = head uniquesBox
               | oneRow      = head uniquesRow
               | oneCol      = head uniquesCol
               | otherwise   = Nothing
               where
                   uniquesBox   = uniques $ boxCoords (x, y) >>= possibilities g
                   uniquesRow   = uniques $ rowCoords y >>= possibilities g
                   uniquesCol   = uniques $ colCoords x >>= possibilities g
                   coordChoices = possibilities g (x, y)
                   onePossible  = length coordChoices == 1
                   oneBox       = length uniquesBox == 1 && (uniquesBox !! 0) `elem` coordChoices
                   oneCol       = length uniquesCol == 1 && (uniquesCol !! 0) `elem` coordChoices
                   oneRow       = length uniquesRow == 1 && (uniquesRow !! 0) `elem` coordChoices
                   solved       = coord g (x, y) /= Nothing

guess :: Grid -> [Grid]
guess g  = guessFirst g coordinates
           where guessFirst :: Grid -> [Coord] -> [Grid]
                 guessFirst g [] = [g]
                 guessFirst g (c:cs) | guessable = map (set g c) possible
                                     | otherwise = guessFirst g cs
                                       where possible  = possibilities g c
                                             guessable = coord g c == Nothing && length possible > 0

solveLoop :: Grid -> Grid
solveLoop g = solveLoop' g []
              where
                  solveLoop' g g' | done g    = g
                                  | g == g'   = tryAll (guess g)
                                  | otherwise = solveLoop' (split 9 $ map (solve g) coordinates) g

tryAll :: [Grid] -> Grid
tryAll gs = tryAll' gs []
            where
                tryAll' [] [g]    = solveLoop g
                tryAll' [g] []    = g
                tryAll' [] (f:fs) | valid g   = g
                                  | otherwise = tryAll' [] fs
                                  where g = solveLoop f
                tryAll' (g:gs) fs | valid s   = s
                                  | otherwise = tryAll' gs (g : fs)
                                  where s = solveLoopNoGuess g

solveLoopNoGuess :: Grid -> Grid
solveLoopNoGuess g = solveLoop' g []
                     where
                         solveLoop' g g' | done g    = g
                                         | g == g'   = g
                                         | otherwise = solveLoop' (split 9 $ map (solve g) coordinates) g

tile :: Maybe Int -> String
tile (Just x)  = show x
tile (Nothing) = " "

solveGrid :: Grid -> IO ()
solveGrid = putStrLn . result

result :: Grid -> String
result g = unlines [prettyPrint g, "", "Solution:", prettyPrint s, "", "Valid: " ++ v]
           where s = solveLoop g
                 v = if valid s then "Yes" else "No"

toGrid :: [String] -> Grid
toGrid = (map (map (\x -> if not (x `elem` ['1'..'9'])
                          then Nothing
                          else Just (digitToInt x)))) . (map (pad 9 '.'))

prettyPrint :: Grid -> String
prettyPrint g = intercalate "\n" $ map (intercalate " | ") $ map (map tile) g

test :: IO ()
test = do mapM (\x -> do putStrLn $ (fst x) ++ ":"
                         (solveGrid . toGrid $ snd x)
                         putStrLn $ (pad 33 '-' "") ++ "\n") allTests
          
          return ()

test17 :: IO ()
test17 = do puzzles <- readFile "sudoku17.txt"
            
            mapM (\x -> do (solveGrid . toGrid $ x)
                           putStrLn $ (pad 33 '-' "") ++ "\n") (map (split 9) (lines puzzles))
            
            return ()

main = do args <- getArgs
          case args of
              ["test"] -> test
              ["test17"] -> test17
              otherwise -> do putStrLn "Type out the grid to solve (anything but 1 thru 9 for empty tiles):"
                              
                              grid <- replicateM 9 getLine
                              
                              putStrLn "\nSolving:"
                              
                              solveGrid . toGrid $ grid
