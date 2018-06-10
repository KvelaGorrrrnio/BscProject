#!/bin/bash

IFS=

for i in 1 #100 200 .. 10000
do
  for j in 1 .. 20 #$i
  do
    code="$code\nl${j}: from l"$(($j-1))"\n  skip\ngoto l"$(("$j+1"))""
  done
  echo $code > hej
done
