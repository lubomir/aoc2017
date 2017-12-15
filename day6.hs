import qualified Data.Set as S

ex, input :: [Int]
ex = [0, 2, 7, 0]

input = [4, 10, 4, 1, 8, 4, 9, 14, 5, 1, 14, 15, 0, 15, 3, 5]

loopLen :: [Int] -> Int
loopLen b0 = go [b0] b0
  where
    go s b = let shifted = step b
             in if shifted `elem` s
                    then 1 + length (takeWhile (/= shifted) s)
                    else go (shifted:s) shifted

countSteps :: [Int] -> Int
countSteps b0 = go (S.singleton b0) b0
  where
    go s b = let shifted = step b
             in if shifted `elem` s
                    then S.size s
                    else go (S.insert shifted s) shifted

showSteps :: [Int] -> Int -> [[Int]]
showSteps b n = map (\x -> iterate step b !! x) [0..n]

step :: [Int] -> [Int]
step b = let (idx, max_) = findMax b
             increments = split (idx + 1) (length b) max_
         in merge idx b increments

merge :: Int -> [Int] -> [Int] -> [Int]
merge _ [] [] = []
merge 0 (_:xs) (y:ys) = y : merge (-1) xs ys
merge n (x:xs) (y:ys) = x + y : merge (n - 1) xs ys


findMax :: [Int] -> (Int, Int)
findMax = go 0 (0, 0)
  where go _ res [] = res
        go cur (idx, max_) (x:xs)
            | x > max_  = go (cur + 1) (cur, x) xs
            | otherwise = go (cur + 1) (idx, max_) xs


split :: Int -> Int -> Int -> [Int]
split off l n = zipWith (+)
                        (replicate l d)
                        (zipWith (+)
                                 (replicate off 0 ++ replicate (min m (l - off)) 1 ++ replicate (l - min m (l - off)) 0)
                                 (replicate (m - min m (l - off)) 1 ++ replicate (l - m + min m (l - off)) 0))
  where
    (d, m) = n `divMod` l
