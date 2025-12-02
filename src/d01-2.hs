import System.Environment
import System.Exit

add ('L' : xs) value = (value - read xs) `mod` 100
add ('R' : xs) value = (value + read xs) `mod` 100

nZeroes ('L' : xs) value
  | newValue == 0 = 1 -- landed on 0
  | otherwise =
      -- check if crossing 0 when going from positive to negative
      abs newValue `div` 100 + if newValue < 0 && value > 0 then 1 else 0
  where
    newValue = value - read xs
nZeroes ('R' : xs) value
  | newValue == 0 = 1 -- landed on 0
  | otherwise = newValue `div` 100
  where
    newValue = value + read xs

solve input =
  snd
    . foldl
      (\(val, zeroes) ins -> (add ins val, zeroes + nZeroes ins val))
      (50, 0)
    $ filter (not . null) (lines input)

parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main = getArgs >>= parse >>= print . solve
