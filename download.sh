#!/bin/bash
if [ $# -ne 1 ]; then
  echo "Usage: $0 <day>"
  exit 1
fi

if=$(printf "%02d\n" "$1")
mkdir -p input
aoc download --year 2025 --day $if --input-only --input-file input/$if.txt
