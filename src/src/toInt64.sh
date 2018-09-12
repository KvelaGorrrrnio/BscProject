#!/bin/bash

find Common -type f -exec sed -i -e "s/Integer/Int64/g" {} \;
find RL     -type f -exec sed -i -e "s/Integer/Int64/g" {} \;
find SRL    -type f -exec sed -i -e "s/Integer/Int64/g" {} \;
stack install
