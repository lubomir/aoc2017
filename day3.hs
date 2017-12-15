module Day3 where

import Data.Maybe
import qualified Data.Map as M

input :: Int
input = 347991

-- |
-- >> sqSize 2
-- 3
-- >> sqSize 5
-- 3
-- >> sqSize 22
-- 6
-- >> sqSize 9
-- 3
sqSize :: Int -> Int
sqSize x = let a' = floor $ sqrt $ (\x -> x - 0.1) $ fromIntegral x
               a = if odd a' then a' + 2 else a' + 1
           in a
           
corners :: Int -> (Int, Int, Int, Int)
corners a =
    ( (a - 2) ^ 2 + a - 1 -- Top right
    , (a - 1) ^ 2 + 1     -- Top left
    , (a - 1) ^ 2 + a     -- Bottom left
    , a ^ 2               -- Bottom right
    )

middles :: Int -> (Int, Int, Int, Int)
middles x = let (a, b, c, d) = corners x
            in ( a - x `div` 2
               , b - x `div` 2
               , c - x `div` 2
               , d - x `div` 2
               )

dist :: Int -> Int
dist 1 = 0
dist x = let a = sqSize x
             (tr, tl, bl, br) = corners a
             (lm, tc, rm, bc) = middles a
         in a `div` 2 + case () of
            _ | x <= tr -> abs (x - lm)
              | x <= tl -> abs (x - tc)
              | x <= bl -> abs (x - rm)
              | x <= br -> abs (x - bc)

----------------------------------------------

-- |
--
-- 17 16 15 14 13
-- 18  5  4  3 12
-- 19  6  1  2 11
-- 20  7  8  9 10
-- 21 22 23 24 25
--
-- >>> pos 1
-- (0,0)
--
-- >>> pos 2
-- (1,0)
--
-- >>> pos 3
-- (1,1)
--
-- >>> pos 4
-- (0,1)
--
-- >>> pos 5
-- (-1,1)
--
-- >>> pos 6
-- (-1,0)
--
-- >>> pos 7
-- (-1,-1)
--
-- >>> pos 8
-- (0,-1)
--
-- >>> pos 9
-- (1,-1)
--
pos :: Int -> (Int, Int)
pos 1 = (0, 0)
pos z = let a = sqSize z
            (tr, tl, bl, br) = corners a
            (rm, tc, lm, bc) = middles a
            x = case () of
                 _ | z <= tr || z == br -> a `div` 2
                   | z <= tl            -> tc - z
                   | z <= bl            -> negate (a `div` 2)
                   | z <  br            -> z - bc
            y = case () of
                 _ | z <= tr -> z - rm
                   | z <= tl -> a `div` 2
                   | z <= bl -> lm - z
                   | z <= br -> negate (a `div` 2)
        in (x, y)


neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]

emptyMemory :: M.Map (Int, Int) Int
emptyMemory = M.singleton (0, 0) 1

atPos :: Num a => M.Map (Int, Int) a -> (Int, Int) -> a
atPos m p = sum (map (\n -> fromMaybe 0 (M.lookup n m)) (neighbours p))

find = go emptyMemory (map pos [1..])
  where
    go m (p:ps) = let v = atPos m p
                  in if v > input
                     then v
                     else go (M.insert p v m) ps
