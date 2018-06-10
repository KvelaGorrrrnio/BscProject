#!/bin/bash

IFS=

echo l, b, i, hm, lin > bench.csv

for ((i=500; i<=7000; i+=500))
do

  iter=50

  code=$(./genprog.sh $i $iter)

  bnum=$(rl blocks <(echo $code))
  lnum=$(echo $code | sed '/^\s*$/d' | wc -l)

  # time hashmap
  pre=$(date +%s%N)
  bin/rl_hm <(echo $code) > /dev/null
  time=$(echo "scale=4; ($(date +%s%N) - $pre)/1000000000" | bc)

  # time linear
  pre=$(date +%s%N)
  bin/rl_lin <(echo $code) > /dev/null
  time2=$(echo "scale=4; ($(date +%s%N) - $pre)/1000000000" | bc)

  # record
  echo $lnum, $bnum, $iter, $time, $time2 >> bench.csv

done
