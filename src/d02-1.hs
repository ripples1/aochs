import Data.Text qualified as T
import Data.Tree (flatten)
import System.Environment (getArgs)
import System.Exit (exitFailure)

toRange s =
  [ read . T.unpack $ low
    .. read . T.unpack $ high
  ] ::
    [Int]
  where
    (low : high : _) = T.split (== '-') s

isInvalid id
  | odd len = False
  | otherwise = let (xs, ys) = splitAt (len `div` 2) id in xs == ys
  where
    len = length id

solve =
  sum
    . concatMap (filter (isInvalid . show) . toRange)
    . T.split (== ',')
    . T.strip
    . T.pack

parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main = getArgs >>= parse >>= print . solve
