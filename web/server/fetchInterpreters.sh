make -C ../../src
if [[ $? -eq 0 ]]; then
  cp -r ../../src/bin .
else
  exit 1
fi
