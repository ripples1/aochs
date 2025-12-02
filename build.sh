#!/bin/bash
if [ $# -ne 1 ]; then
  echo "Usage: $0 <source-file>"
  exit 1
fi
fn=${1##*/}
mkdir -p bin
ghc -O -outputdir bin -o bin/${fn%.*} $1 