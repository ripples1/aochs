import Data.List (nub)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import System.Environment (getArgs)
import System.Exit (exitFailure)

toRange s =
  [ read . T.unpack $ low
    .. read . T.unpack $ high
  ] ::
    [Int]
  where
    (low : high : _) = T.split (== '-') s

repeatsN id n
  | m == 0 = ((== 1) . length . nub . T.chunksOf d) id
  | otherwise = False
  where
    (d, m) = T.length id `divMod` n

isInvalid id = any (repeatsN id) [2 .. T.length id]

solve :: String -> Int
solve =
  sum
    . concatMap (filter (isInvalid . T.show) . toRange)
    . T.split (== ',')
    . T.strip
    . T.pack

parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main = getArgs >>= parse >>= print . solve
