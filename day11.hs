module Main where

import           Data.List

data Coord = Coord { x, y, z :: Int } deriving (Show, Eq)

origin :: Coord
origin = Coord 0 0 0

step :: String -> Coord
step "n"   = Coord 0    1    (-1)
step "ne"  = Coord 1    0    (-1)
step "se"  = Coord 1    (-1) 0
step "s"   = Coord 0    (-1) 1
step "sw"  = Coord (-1) 0    1
step "nw"  = Coord (-1) 1    0
step _     = error "This is not possible..."

add :: Coord -> Coord -> Coord
add (Coord x1 y1 z1) (Coord x2 y2 z2) = Coord (x1 + x2) (y1 + y2) (z1 + z2)

dist :: Coord -> Coord -> Int
dist (Coord x1 y1 z1) (Coord x2 y2 z2) =
    (abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)) `div` 2

main :: IO ()
main = do
    input <- map step . words . map (\c -> if c == ',' then ' ' else c) <$> readFile "directions.txt"
    let positions = scanl' add origin input
    let distances = map (dist origin) positions
    putStrLn $ "Final distance: " ++ show (last distances)
    putStrLn $ "Max distance:   " ++ show (maximum distances)
