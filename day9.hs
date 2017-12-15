data InGarbage = In | Out deriving (Eq, Show)

cleanGarbage :: String -> (String, Int)
cleanGarbage = go (Out, 0)
  where
    go (Out, s) [] = ([], s)
    go (Out, s) ('<':xs) = go (In, s) xs
    go (Out, s) (x:xs) = let (cleaned, removed) = go (Out, s) xs
                         in (x:cleaned, removed)
    go (In, s)  ('!':_:xs) = go (In, s) xs
    go (In, s)  ('>':xs) = go (Out, s) xs
    go (In, s)  (_:xs) = go (In, s + 1) xs

level :: String -> Int
level = go 0
  where
    go _ [] = 0
    go l ('{':xs) = go (l + 1) xs
    go l ('}':xs) = l + go (l - 1) xs
    go l (',':xs) = go l xs


main :: IO ()
main = do
    stream <- init <$> readFile "garbage.txt"
    let (cleaned, removed) = cleanGarbage stream
    print $ level cleaned
    print removed
