# Reversible Interpreters

This project concerns itself with implementing two reversible languages (RL): an unstructured (RL) and a structured (SRL).
These are written in Haskell, and are found ind /src.
A webinterface for running and interacting with the interpreters, are found under /web.

# Getting Started

## Prerequisites

### Interpreters
For building the interpreters, haskell-stack is required.
```bash
brew install haskell-stack cabal-install ghc # MacOS
curl -sSL https://get.haskellstack.org/ | sh # Unix
wget -qO- https://get.haskellstack.org/ | sh # Unix alternative
```
For windows see [stack documentation](https://docs.haskellstack.org/en/stable/README/).

### Webinterface
For building and runnning the webinterface node is required.
```bash
brew install node # Mac OS
sudo apt-get install nodejs npm # Ubuntu
sudo pacman -S nodejs npm # Arch
```

## Installing

### Interpreters

The interpreters can be built with
```bash
make src # Binaries can be found in /src/bin
```

For installing binaries to local bin
```bash
make install
```

### Webinterface
The webinterface can be built with
```bash
make web
```

## Usage

### Interpreters

### Webinterface

For starting the webserver run
```bash
make server # Starts server at http://localhost:3001/
```
