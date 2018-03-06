#!/usr/bin/env bash
curdir="$(cd "$(dirname "$0")" && pwd)"
target=("rl" "srl")
details=0
while [ ! $# -eq 0 ]; do
  case "$1" in
    "rl" )
      target=("rl")
      ;;
    "srl" )
      target=("srl")
      ;;
    "test" )
      target=("test")
      ;;
    -d )
      details=1
      ;;
    * )
      echo "Unknown target."
      exit
  esac
  shift
done
for t in "${target[@]}"; do
  files="$(ls $curdir/$t/*.hs 2> /dev/null)"
  printf "\033[0;35mRunning tests for $t:\033[0;0m\n"
  for f in ${files[@]}; do
    if [ ! -f "${f%.*}.hso" ]; then
      printf "\033[0;31m$(basename $f): No output file found.\033[0;0m\n"
    else
      r=$(runhaskell -i$curdir/../${t} $f 2>err)
      c=$?
      o=$(cat "${f%.*}.hso")
      if [ $c -ne 0 ]; then
        printf "\033[0;31m$(basename $f): Compile failed"
        if [ $details -eq 1 ]; then
          printf ":\033[0;0m$(cat err)\n\n"
        else
          printf "\n"
        fi
      elif [ "$r" == "$o" ]; then
        printf "\033[0;32m$(basename $f): Success\033[0;0m\n"
      else
        printf "\033[0;31m$(basename $f): Failed."
        if [ $details -eq 1 ]; then
          printf " Expected:\n\033[0;0m$o\033[0;31m\n, but got:\n\033[0;0m$r\n\n"
        else
          printf "\n"
        fi
      fi
    fi
  done
  printf "\n"
done
rm err
