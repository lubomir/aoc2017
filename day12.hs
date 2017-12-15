module Main where

import           Data.Char
import           Data.List
import qualified Data.Map.Lazy   as M
import qualified Data.Set        as S

parse :: String -> (Int, S.Set Int)
parse l = let (w:_:xs) = words l
          in (read w, S.fromList $ map (read . filter isDigit) xs)

findGroup :: M.Map Int (S.Set Int) -> S.Set Int -> Int -> S.Set Int
findGroup g visited x = case M.lookup x g of
    Nothing -> error "This is not possible"
    Just xs -> let vs = visited `S.union` xs
               in foldl' (findGroup g) vs (xs `S.difference` visited)

countGroups :: M.Map Int (S.Set Int) -> Int
countGroups = go 0
  where
    go acc m
      | M.null m = acc
      | otherwise = let g = findGroup m S.empty (head $ M.keys m)
                    in go (acc + 1) (M.filterWithKey (\k _ -> k `S.notMember` g) m)

main :: IO ()
main = do
    inp <- M.fromList . map parse . lines <$> readFile "village.txt"
    putStrLn $ "Number of programs in group 0: " ++ show (S.size $ findGroup inp S.empty 0)
    putStrLn $ "Number of groups: " ++ show (countGroups inp)
