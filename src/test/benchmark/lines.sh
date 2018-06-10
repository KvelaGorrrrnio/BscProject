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
  echo t, l, rl_hm, rl_lin, srl > $filename.csv

  for i in {0..6}
  do
    # fetch number of blocks
    num=$($interp blocks <(echo $code) )

    # change interpreter and record
    if [ $interp = "rl" ]
    then
      # time hm
      pre=$(date +%s%N)
      bin/rl_hm <(echo $code) > /dev/null
      time=$(echo "scale=4; ($(date +%s%N) - $pre)/1000000000" | bc)
      # time lin
      pre=$(date +%s%N)
      bin/rl_lin <(echo $code) > /dev/null
      time2=$(echo "scale=4; ($(date +%s%N) - $pre)/1000000000" | bc)

      # record
      echo $i, $num, $time, $time2, {}  >> $filename.csv

      # translate
      code=$($interp translate <(echo $code) )

      # change interp
      interp="srl"
    else
      # time std
      pre=$(date +%s%N)
      $interp <(echo $code) > /dev/null
      time=$(echo "scale=4; ($(date +%s%N) - $pre)/1000000000" | bc)

      # record
      echo $i, $num, {}, {}, $time >> $filename.csv

      # translate
      code=$($interp translate <(echo $code) )

      # change interp
      interp="rl"
    fi

  done
done
