make -C ../client
if [[ $? -eq 0 ]]; then
  cp -r ../client/build ./client
else
  exit 1
fi
