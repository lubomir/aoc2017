module Main where

import           Data.Bits
import           Data.Char
import           Data.List
import           Data.List.Split
import           Test.DocTest
import           Text.Printf

strInp :: String
strInp = "199,0,255,136,174,254,227,16,51,85,1,2,22,17,7,192"

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

main :: IO ()
main = print $ hash strInp

test = doctest ["day10.hs"]
