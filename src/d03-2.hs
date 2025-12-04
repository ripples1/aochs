import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)

joltage :: String -> Int -> Maybe Int
joltage bank n = (fmap (read . fst) . foldM joltageDigit ("", bank)) [1 .. n]
  where
    joltageDigit (d, bank) k =
      ( fmap (first ((d ++) . pure))
          . listToMaybe
          . dropWhile ((< n - k) . length . snd)
          . Map.toDescList
          . Map.map (\idx -> drop (idx + 1) bank)
          . Map.fromListWith min
      )
        (zip bank [0 ..])

solve = sum . map (fromMaybe 0 . (`joltage` 12)) . filter (not . null) . lines

parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main = getArgs >>= parse >>= print . solve
