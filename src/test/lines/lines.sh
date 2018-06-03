#!/bin/bash

IFS=

for f in *rl
do
  start=$f
  filename=$(basename -- "$f")
  interp="${filename##*.}"
  filename="${filename%.*}"

  # first iteration
  code=$(cat $start)
  num=$(echo $code | sed '/^\s*$/d' | wc -l)
  echo t, l    >  $filename.csv
  echo 0, $num >> $filename.csv

  for i in {1..5}
  do
    code=$(/home/larsvadgaard/.local/bin/$interp translate <(echo $code) )
    num=$(echo $code | sed '/^\s*$/d' | wc -l)
    echo $i, $num >> $filename.csv

    if [ $interp = "rl" ]
    then
      interp="srl"
    else
      interp="rl"
    fi

  done
done
