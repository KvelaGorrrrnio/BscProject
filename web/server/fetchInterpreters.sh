stackbin=$(cd ../../src && stack path --local-install-root)/bin
cd ../../src
echo "Building RL and SRL interpreters..."
stack build >/dev/null 2>&1
if [[ $? -eq 0 ]]; then
  echo "rl and srl have been put in ./bin."
  cd ../web/server
  mkdir -p ./bin
  cp $stackbin/* ./bin
else
  echo "rl and srl won't build due to errors."
  exit 1
fi
