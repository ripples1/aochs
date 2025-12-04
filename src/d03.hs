import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import System.Environment (getArgs)
import System.Exit (exitFailure)

joltage n bank = (read . fst . foldl joltageDigit ("", bank)) [1 .. n]
  where
    joltageDigit (d, bank) k =
      ( first ((d ++) . pure)
          . head
          . dropWhile ((< n - k) . length . snd)
          . Map.toDescList
          . Map.map (\idx -> drop (idx + 1) bank)
          . Map.fromListWith min
      )
        (zip bank [0 ..])

solve = sum . map (joltage 12) . filter (not . null) . lines

parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main = getArgs >>= parse >>= print . solve
