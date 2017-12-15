module Main where

--import           Data.Char
import           Data.Maybe
import qualified Data.Map.Strict as M
import           Test.DocTest
--import qualified Data.Set        as S

-- |
-- >> map (flip posAtTime 2) [0..5]
-- [0,1,0,1,0,1]
-- >>> map (flip posAtTime 3) [0..10]
-- [0,1,2,1,0,1,2,1,0,1,2]
-- >>> map (flip posAtTime 4) [0..10]
-- [0,1,2,3,2,1,0,1,2,3,2]
-- >>> map (posAtTime 0) [2,3,4,5,6]
-- [0,0,0,0,0]
-- >>> map (posAtTime 1) [2,3,4,5,6]
-- [1,1,1,1,1]
-- >>> map (posAtTime 2) [2,3,4,5,6]
-- [0,2,2,2,2]
-- >>> map (posAtTime 3) [2,3,4,5,6]
-- [1,1,3,3,3]
-- >>> map (posAtTime 4) [2,3,4,5,6]
-- [0,0,2,4,4]
-- >>> map (posAtTime 5) [2,3,4,5,6]
-- [1,1,1,3,5]
-- >>> map (posAtTime 6) [2,3,4,5,6]
-- [0,2,0,2,4]
-- >>> map (posAtTime 7) [2,3,4,5,6]
-- [1,1,1,1,3]
-- >>> map (posAtTime 8) [2,3,4,5,6]
-- [0,0,2,0,2]
-- >>> map (posAtTime 9) [2,3,4,5,6]
-- [1,1,3,1,1]
-- >>> map (posAtTime 10) [2,3,4,5,6]
-- [0,2,2,2,0]
posAtTime :: Int -> Int -> Int
posAtTime t r = cycle ([0..r-1] ++ reverse [1..r-2]) !! (t `mod` (2 * r - 2))

sumMaybe :: [Maybe Int] -> Maybe Int
sumMaybe xs = case catMaybes xs of
    [] -> Nothing
    ys -> Just (sum ys)

-- |
-- >>> penalty 0 input
-- Just 2688
-- >>> penalty 0 ex
-- Just 24
penalty :: Int -> M.Map Int Int -> Maybe Int
penalty t s = sumMaybe $ M.elems $ M.mapWithKey (\k r -> if posAtTime (t + k) r == 0 then Just (k * r) else Nothing) s

-- |
-- >>> minPenalty ex
-- 10
minPenalty :: M.Map Int Int -> Int
minPenalty inp = fst $ head $ dropWhile (isJust . snd) $ map (\d -> (d, penalty d inp)) [0..]


main :: IO ()
main =
    mapM_ printLine $ takeWhile (isJust . snd) $ map (\d -> (d, penalty d input)) [0,2..]
  where
    printLine (d, p) = putStrLn (show d ++ "\t" ++ show p)


input :: M.Map Int Int
input = M.fromList [ (0,  4 )
                   , (1,  2 )
                   , (2,  3 )
                   , (4,  5 )
                   , (6,  6 )
                   , (8,  4 )
                   , (10, 8 )
                   , (12, 6 )
                   , (14, 6 )
                   , (16, 8 )
                   , (18, 8 )
                   , (20, 6 )
                   , (22, 8 )
                   , (24, 9 )
                   , (26, 8 )
                   , (28, 8 )
                   , (30, 12)
                   , (32, 12)
                   , (34, 10)
                   , (36, 12)
                   , (38, 12)
                   , (40, 10)
                   , (42, 12)
                   , (44, 12)
                   , (46, 12)
                   , (48, 12)
                   , (50, 12)
                   , (52, 14)
                   , (54, 14)
                   , (56, 12)
                   , (58, 14)
                   , (60, 14)
                   , (62, 14)
                   , (64, 17)
                   , (66, 14)
                   , (70, 14)
                   , (72, 14)
                   , (74, 14)
                   , (76, 14)
                   , (78, 18)
                   , (82, 14)
                   , (88, 18)
                   , (90, 14)
                   ]

ex :: M.Map Int Int
ex = M.fromList [(0, 3), (1, 2), (4, 4), (6, 4)]

{-
 - 0 0 - - 0 - 0
 - 1 1 - - 1 - 1
 - 2 0 - - 2 - 2
 - 1 1 - - 3 - 3
 - 0 0 - - 2 - 2
 - 1 1 - - 1 - 1
 - 2 0 - - 0 - 0
 - 1 1 - - 1 - 1
 - 0 0 - - 2 - 2
 - 1 1 - - 3 - 3
 - 2 0 - - 2 - 2
 -}

test :: IO ()
test = doctest ["day13.hs"]
