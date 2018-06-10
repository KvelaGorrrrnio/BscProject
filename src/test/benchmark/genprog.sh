#!/bin/bash

IFS=

code="int n\n\nl1: entry\n  n += $2\ngoto l2"
code="${code}\nl2: fi n = $2 l1 l$(($1 - 1))\n  skip\ngoto l3"

max=$(($1 - 2))
for ((j=3;j<=$max;j++))
do
  prelab=l$(( $j - 1 ))
  curlab=l$j
  poslab=l$(( $j + 1 ))
  code="$code\n$curlab: from $prelab\n  skip\ngoto $poslab"
done

prelab=l$(( $j - 1 ))
curlab=l$j
poslab=l$(( $j + 1 ))
code="$code\n$curlab: from $prelab\n  n -= 1\nif n = 0 $poslab l2"

code="$code\n$poslab: from $curlab\n  skip\nexit\n"

printf $code
