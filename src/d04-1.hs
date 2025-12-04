{- HLINT ignore "Use tuple-section" -}
import Data.Complex
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
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
    (\n -> fmap (\v -> (c + n, v)) (g Map.!? (c + n)))
    neighbourCoords

mapNeighbours :: Char -> Coord -> Grid -> Maybe [(Coord, Char)]
mapNeighbours '@' c g = Just rolls
  where
    rolls = (filter ((== '@') . snd) . neighbours c) g
mapNeighbours _ _ _ = Nothing

-- solve :: String -> Int
solve s =
  ( length
      . filter ((< 4) . length)
      . catMaybes
      . Map.elems
      . Map.mapWithKey (\c v -> mapNeighbours v c g)
  )
    g
  where
    g = toGrid s

parse :: [FilePath] -> IO String
parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main :: IO ()
main = getArgs >>= parse >>= print . solve
