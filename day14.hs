module Main where

import           Data.Bits
import           Data.Char
import           Data.List
import           Data.List.Split
import           Test.DocTest
import           Text.Printf
import qualified Data.Set as S

key, ex :: String
key = "ffayrhll-"
ex = "flqrgnkx-"

-- |
-- >>> parseInput "1,2,3"
-- [49,44,50,44,51,17,31,73,47,23]
parseInput :: String -> [Int]
parseInput inp = map ord inp ++ [17, 31, 73, 47, 23]


-- |
-- >>> circularReverse [0..3] 0 2
-- [1,0,2,3]
-- >>> circularReverse [0..3] 1 2
-- [0,2,1,3]
-- >>> circularReverse [0..3] 2 2
-- [0,1,3,2]
-- >>> circularReverse [0..3] 3 2
-- [3,1,2,0]
--
-- prop> \l -> circularReverse l 0 (length l) == reverse l
circularReverse :: [a] -> Int -> Int -> [a]
circularReverse l off len =
    let (pref, suff) = splitAt off (cycle l)
        (toRev, rest) = splitAt len suff
        reconstructed = pref ++ reverse toRev ++ rest
        newPref = take (length pref) $ drop (length l) reconstructed
    in take (length l) (newPref ++ reverse toRev ++ rest)
        

-- |
-- >>> step ([0, 1, 2, 3, 4], 0, 0) 3
-- ([2,1,0,3,4],3,1)
-- >>> step ([2,1,0,3,4],3,1) 4
-- ([4,3,0,1,2],3,2)
-- >>> step ([4,3,0,1,2],3,2) 1
-- ([4,3,0,1,2],1,3)
-- >>> step ([4,3,0,1,2],1,3) 5
-- ([3,4,2,1,0],4,4)
step :: ([Int], Int, Int) -> Int -> ([Int], Int, Int)
step (l, pos, skip) len =
    (circularReverse l pos len, (pos + skip + len) `mod` length l, skip + 1)

oneRound :: ([Int], Int, Int) -> [Int] -> ([Int], Int, Int)
oneRound = foldl' step

sparseHash :: String -> [Int]
sparseHash input = 
    let initialState = ([0..255], 0, 0)
        (res, _, _) = foldl' oneRound initialState (replicate 64 (parseInput input))
    in res

denseHash :: [Int] -> [Int]
denseHash = map (foldl' xor 0) . chunksOf 16

-- |
-- >>> fmt [64, 7, 255]
-- "4007ff"
fmt :: [Int] -> String
fmt = concatMap (printf "%02x")


-- |
-- >>> hash ""
-- "a2582a3a0e66e6e86e3812dcb672a272"
-- >>> hash "AoC 2017"
-- "33efeb34ea91902bb2f59c9920caa6cd"
-- >>> hash "1,2,3"
-- "3efbe78a8d82f29979031a4aa0b16a9d"
-- >>> hash "1,2,4"
-- "63960835bcdc130f0b66d7ff4f6a5a8e"
hash :: String -> String
hash = fmt . denseHash . sparseHash

hexDigitToInt :: Char -> Int
hexDigitToInt c
    | isDigit c = ord c - ord '0'
    | otherwise = ord c - ord 'a' + 10

sumBits :: Int -> Int
sumBits 0 = 0
sumBits x = sumBits (x `div` 2) + if odd x then 1 else 0

bits :: Int -> [Bool]
bits x = map (testBit x) [3,2..0]

used :: String -> Int
used k = sum $ concatMap (map (sumBits . hexDigitToInt) . hash . (k ++) . show) [(0::Int)..127]

matrix :: String -> S.Set (Int, Int)
matrix k = S.fromList
           [ (r, c)
           | (r, h) <- map (\i -> (i, hash $ (k ++) $ show i)) [(0::Int)..127]
           , (c, x) <- zip [0..] (concatMap (bits . hexDigitToInt) h)
           , x
           ]

nextTo :: (Int, Int) -> (Int, Int) -> Bool
nextTo (a,b) (c,d) = abs (a-c) + abs (b-d) <= 1

extractGroup :: S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
extractGroup positions x =
    let (neighbours, rest) = S.partition (nextTo x) positions
    in foldl' extractGroup rest neighbours

countGroups :: S.Set (Int, Int) -> Int
countGroups positions
  | S.null positions = 0
  | otherwise = let (x, positions') = S.deleteFindMin positions
                in 1 + countGroups (extractGroup positions' x)

main :: IO ()
main = do
    print $ used key
    print $ countGroups $ matrix key

test :: IO ()
test = doctest ["day10.hs"]

showMatrix :: Int -> S.Set (Int, Int) -> IO ()
showMatrix n = mapM_ putStrLn . showMatrix' n

showMatrix' :: Int -> S.Set (Int, Int) -> [String]
showMatrix' n m = [[ if (x, y) `S.member` m then 'X' else '-'
                   | y <- [0..n]
                   ]
                  | x <- [0..n]
                  ]
