#!/bin/bash
if [ $# -ne 1 ]; then
  echo "Usage: $0 <filename>"
  exit 1
fi

fn=${1##*/}
fnn=${fn%.*}

mkdir -p src
cat >src/${fnn}.hs <<EOL
import System.Environment
import System.Exit

solve = id

parse [] = putStrLn "Input file not specified" >> exitFailure
parse (f : _) = readFile f

main = getArgs >>= parse >>= print . solve
EOL