import Data.Complex
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)

type Coord = Complex Float

type Grid = HashMap Coord Char

toGrid :: String -> Grid
toGrid =
  Map.fromList
    . concatMap
      (\(y, line) -> zipWith (\x c -> (x :+ y, c)) [0 ..] line)
    . zip [0 ..]
    . filter (not . null)
    . lines

neighbourCoords =
  [ (-1) :+ (-1),
    0 :+ (-1),
    1 :+ (-1),
    (-1) :+ 0 {-  it me  -},
    1 :+ 0,
    (-1) :+ 1,
    0 :+ 1,
    1 :+ 1
  ]

neighbours :: Coord -> Grid -> [(Coord, Char)]
neighbours c g =
  mapMaybe
    (\n -> fmap (c + n,) (g Map.!? (c + n)))
    neighbourCoords

accessibleRolls :: Char -> Coord -> Grid -> Maybe [(Coord, Char)]
accessibleRolls '@' c g =
  if length rolls < 4
    then
      Just rolls
    else Nothing
  where
    rolls = (filter ((== '@') . snd) . neighbours c) g
accessibleRolls _ _ _ = Nothing

removeRolls :: Grid -> (Int, Grid)
removeRolls g = go (0, g)
  where
    go (removed, g)
      | null rollsToRemove = (removed, g)
      | otherwise = go (removed + length rollsToRemove, Map.difference g rollsToRemove)
      where
        rollsToRemove = Map.filterWithKey (\c v -> isJust $ accessibleRolls v c g) g

solve = fst . removeRolls . toGrid

parse :: [FilePath] -> IO String
parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main :: IO ()
main = getArgs >>= parse >>= print . solve
