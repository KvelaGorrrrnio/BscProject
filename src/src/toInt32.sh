#!/bin/bash

find Common -type f -exec sed -i -e "s/Integer/Int32/g" {} \;
find RL     -type f -exec sed -i -e "s/Integer/Int32/g" {} \;
find SRL    -type f -exec sed -i -e "s/Integer/Int32/g" {} \;
stack install
