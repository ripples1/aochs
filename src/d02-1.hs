import Data.Tree (flatten)
import Distribution.Utils.String (trim)
import System.Environment (getArgs)
import System.Exit (exitFailure)

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
  "" -> []
  s' -> w : split p s''
    where
      (w, s'') = break p s'

toRange s = [read l .. read h] :: [Int]
  where
    (l : h : _) = split (== '-') s

csv = split (== ',')

isInvalid id
  | odd len = False
  | otherwise = let (xs, ys) = splitAt (len `div` 2) id in xs == ys
  where
    len = length id

solve = sum . concatMap (filter (isInvalid . show) . toRange) . csv . trim

parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main = getArgs >>= parse >>= print . solve
