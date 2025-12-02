import Data.List (nub)
import Distribution.Utils.String (trim)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- from Data.List.Split
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

-- from Data.List.Split
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
  "" -> []
  s' -> w : split p s''
    where
      (w, s'') = break p s'

toRange s = [read l .. read h] :: [Int]
  where
    (l : h : _) = split (== '-') s

repeatsN id n
  | m == 0 = ((== 1) . length . nub . chunksOf d) id
  | otherwise = False
  where
    (d, m) = length id `divMod` n

isInvalid id = any (repeatsN id) [2 .. length id]

csv = split (== ',')

solve = sum . concatMap (filter (isInvalid . show) . toRange) . csv . trim

parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main = getArgs >>= parse >>= print . solve
