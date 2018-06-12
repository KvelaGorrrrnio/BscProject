#!/bin/bash

find Common -type f -exec sed -i -e "s/Int32/Integer/g" {} \;
find RL     -type f -exec sed -i -e "s/Int32/Integer/g" {} \;
find SRL    -type f -exec sed -i -e "s/Int32/Integer/g" {} \;
stack install
