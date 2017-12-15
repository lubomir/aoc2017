import qualified Data.Map.Strict as M
import Data.List
import Debug.Trace


data Instr = Instr { reg :: String
                   , op :: Int -> Int -> Int
                   , diff :: Int
                   , check :: String
                   , test :: Int -> Bool
                   , orig :: String
                   }

instance Show Instr where
    show = orig

showMemory :: M.Map String Int -> String
showMemory m = unwords [k ++ ":" ++ show v | (k, v) <- M.assocs m]

parseOp :: String -> Int -> Int -> Int
parseOp "inc" = (+)
parseOp "dec" = (-)

parseCmp :: String -> Int -> Int -> Bool
parseCmp ">" = (>)
parseCmp ">=" = (>=)
parseCmp "<" = (<)
parseCmp "<=" = (<=)
parseCmp "==" = (==)
parseCmp "!=" = (/=)

parse :: String -> Instr
parse l = let [reg, op', diff', _, check, cmp, val] = words l
          in Instr reg (parseOp op') (read diff') check (flip (parseCmp cmp) (read val)) l

eval :: (M.Map String Int, Int) -> Instr -> (M.Map String Int, Int)
eval (m, m0) i = (show i ++ "\t" ++ showMemory m) `trace`
    let old = M.findWithDefault 0 (reg i) m
        toCheck = M.findWithDefault 0 (check i) m
        new = op i old (diff i)
    in if test i toCheck
        then (M.insert (reg i) new m, max m0 new)
        else (m, m0)

main :: IO ()
main = do
    instructions <- map parse . lines <$> readFile "instructions.txt"
    let (memory, m0) = foldl' eval (M.empty, 0) instructions
    print $ maximum $ M.elems memory
    print m0
