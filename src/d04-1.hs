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

{- ORMOLU_DISABLE -}
neighbourCoords =
  [
    (-1) :+ (-1),  0 :+ (-1),  1 :+ (-1),
    (-1) :+   0 ,              1 :+   0 ,
    (-1) :+   1 ,  0 :+   1,   1 :+   1
  ]
{- ORMOLU_ENABLE -}

neighbours :: Coord -> Grid -> [(Coord, Char)]
neighbours c g =
  mapMaybe
    (\n -> fmap (c + n,) (g Map.!? (c + n)))
    neighbourCoords

mapNeighbours :: Char -> Coord -> Grid -> Maybe [(Coord, Char)]
mapNeighbours '@' c g = Just rolls
  where
    rolls = (filter ((== '@') . snd) . neighbours c) g
mapNeighbours _ _ _ = Nothing

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

parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main :: IO ()
main = getArgs >>= parse >>= print . solve
