#!/bin/bash

IFS=

code="int n\nn += $2\nfrom (n=$2) do"

max=$(($1 - 4))
for ((j=1;j<=$max;j++))
do
  code="$code\n  skip"
done

code="$code\n  n -= 1\nloop skip\nuntil (n=0)\nskip\n"

printf $code
