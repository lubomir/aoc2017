{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.Maybe (fromJust)
import Data.List (sort)
import Debug.Trace

data Program = Program { name :: T.Text
                       , weight :: Int
                       , children :: [T.Text]
                       }
                       deriving (Show)

parse :: T.Text -> Program
parse l = case T.splitOn " -> " l of
            [nw, cs'] -> let (name, w) = parseName nw
                             cs = T.splitOn ", " cs'
                         in Program name w cs
            _ -> let (name, w) = parseName l
                 in Program name w []
  where
    parseName nw = let [name, w'] = T.splitOn " (" nw
                   in (name, read $ T.unpack $ T.init w')

findTop :: M.Map T.Text T.Text -> T.Text -> T.Text
findTop m p = case M.lookup p m of
    Nothing -> p
    Just parent -> findTop m parent

towerWeight :: M.Map T.Text Program -> Program -> Int
towerWeight m p =
    let childWeights = map (towerWeight m . fromJust . flip M.lookup m) (children p)
    in weight p + sum childWeights


-- isBalanced :: M.Map T.Text Program -> Program
isBalanced m p = trace (show p) $ case children p of
    [] -> Right (weight p)
    cs -> joinResults p (weight p) $ map (isBalanced m . fromJust . flip M.lookup m) cs

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

fromRight :: Show a => Either a b -> b
fromRight (Right x) = x
fromRight (Left x) = error $ "Got Left " ++ show x

count :: [Int] -> [(Int, Int)]
count xs = M.assocs $ M.fromListWith (+) [(x, 1) | x <- xs]

joinResults :: Program -> Int -> [Either (Program, Int, Int) Int] -> Either (Program, Int, Int) Int
joinResults n w0 res = trace (T.unpack (name n) ++ ": " ++ show res ++ ": " ++ show r) r
  where
    r = case filter isLeft res of
            [x] -> x
            _ -> go $ count $ sort $ map fromRight res
    go [(w, c)] = Right (w * c + w0)
    go ws = Left (n, fst $ head $ filter ((== 1) . snd) ws, fst $ head $ filter ((/= 1) . snd) ws)

main :: IO ()
main = do
    progs <- map parse . T.lines <$> IO.readFile "programs.txt"
    let m = M.fromList [(name p, p) | p <- progs]
    let parents = M.fromList [(c, name p) | p <- progs, c <- children p]
    let top = findTop parents (name $ head progs)
    let Left (unbalanced, wrong, correct) = isBalanced m (fromJust $ M.lookup "vwkkml" m)
    print $ "Node " ++ T.unpack (name unbalanced)
    --map (towerWeight m . fromJust . flip M.lookup m) (children
