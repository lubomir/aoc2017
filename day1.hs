import Data.Char

captcha :: [Int] -> Int
captcha x = sum $ zipWith merge x (drop (length x `div` 2) (cycle x))
  where
    merge a b
      | a == b    = a
      | otherwise = 0

parse :: String -> [Int]
parse = map digitToInt
