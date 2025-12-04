import System.Environment (getArgs)
import System.Exit (exitFailure)

add ('L' : xs) value = (value - read xs) `mod` 100
add ('R' : xs) value = (value + read xs) `mod` 100

solve input =
  snd
    . foldl
      ( \(val, zeroes) ins ->
          let nextValue = add ins val
           in (nextValue, zeroes + if nextValue == 0 then 1 else 0)
      )
      (50, 0)
    $ filter (not . null) (lines input)

parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main = getArgs >>= parse >>= print . solve
