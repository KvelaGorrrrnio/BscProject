#!/bin/bash

IFS=

echo l, b, i, rl_hm, rl_lin, srl > bm.csv

for ((i=500; i<=10000; i+=500))
do

  # number of iterations in the programs
  iter=50

  # generate the RL program
  rlcode=$(./genrlprog.sh $i $iter)
  # generate the SRL program
  srlcode=$(./gensrlprog.sh $i $iter)

  # number of blocks
  bnum=$i
  # number of lines
  lnum=$(echo $rlcode | sed '/^\s*$/d' | wc -l)

  # time hashmap
  pre=$(date +%s%N)
  bin/rl_hm <(echo $rlcode) > /dev/null
  rltime=$(echo "scale=4; ($(date +%s%N) - $pre)/1000000000" | bc)

  # time linear
  pre=$(date +%s%N)
  bin/rl_lin <(echo $rlcode) > /dev/null
  rltime2=$(echo "scale=4; ($(date +%s%N) - $pre)/1000000000" | bc)

  # time srl
  pre=$(date +%s%N)
  bin/srl <(echo $srlcode) > /dev/null
  srltime=$(echo "scale=4; ($(date +%s%N) - $pre)/1000000000" | bc)

  # record
  echo $lnum, $bnum, $iter, $rltime, $rltime2, $srltime >> bm.csv

done
