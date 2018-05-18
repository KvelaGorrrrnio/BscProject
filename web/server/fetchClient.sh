cd ../client
echo "Building Client web interface..."
npm run build >/dev/null 2>&1
if [[ $? -eq 0 ]]; then
  echo "Client web interface has been put in ./client."
  cd ../server
  mkdir -p ./client
  cp ../client/build/* ./client
else
  echo "Client web interface won't build due to errors."
  exit 1
fi
