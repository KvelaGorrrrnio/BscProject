# Command Line Interface

## Prerequisites

To build and run the interpreters, haskell-stack is required.
```bash
brew install haskell-stack cabal-install ghc # MacOS
curl -sSL https://get.haskellstack.org/ | sh # Unix
wget -qO- https://get.haskellstack.org/ | sh # Unix alternative
```
For Windows see [stack documentation](https://docs.haskellstack.org/en/stable/README/).

## Build

Firstly clone the github repository
```bash
git clone https://github.com/KvelaGorrrrnio/BscProject.git bsc
cd bsc
```

The interpreters can be built with
```bash
make src # Binaries can be found in /src/bin
```

To install the binaries to local bin, run
```bash
make install
```

## Usage

#### Help
To display the help message, run either `rl` or `srl` providing the `--help` flag.
```bash
The Glorious [S]RL Interpreter System, version 1.0.0

[s]rl [COMMAND] ... [OPTIONS]
  Interpret, invert or translate an [S]RL program

  Common flags:
    -o --out=FILE         Write the output to the specified file
    -j --json             Format the output as JSON
    -c --code             Give a string to be treated as [S]RL code
    -h --help             Display help message
    -v --version          Print version information
       --numeric-version  Print just the version number

   [s]rl [run] [OPTIONS] [FILE]
   Interpret an [S]RL program

   -l --log              Output log instead of final state

   [s]rl invert [OPTIONS] [FILE]
     Invert an [S]RL program

   [s]rl translate [OPTIONS] [FILE]
     Translate a program in one language to a program in the other language

   [s]rl blocks [OPTIONS] [FILE]
     Print the number of blocks in the program
```

#### Autocompletion

```bash
autoload bashcompinit # ZSH only
bashcompinit          # ZSH only

function _autocomplete_srl_or_rl() {
  if [[ $# != 3 ]]; then
    return;
  fi
  local lng="$1"
  local allmodes="run translate invert blocks"
  local prev="${COMP_WORDS[COMP_CWORD - 1]}"
  local cur="${COMP_WORDS[COMP_CWORD]}"
  local files=( $(IFS=' '; find . -maxdepth 1 -name "${cur}*.${lng}") )
  if [[ -f "${cur}${lng}" ]]; then
    files=( "${cur}${lng}" )
  fi
  if [[ "$prev"  = "${lng}" ]]; then
    local modes=( $(compgen -W "$allmodes" -- "$cur") )
    COMPREPLY=( ${modes[@]} ${files[@]/.\//} )
  elif [[ "$allmodes" =~ "$prev" ]]; then
    COMPREPLY=( ${files[@]/.\//} )
  fi
}
complete -F _autocomplete_srl_or_rl "rl"  rl
complete -F _autocomplete_srl_or_rl "srl" srl
```

#### Syntax highlighting

We have defined syntax highlighting for each of the two languages for Vim. Move `vim/syntax/(s)rl.vim` to `.vim/syntax` and `vim/ftdetect/(s)rl.vim` to `.vim/ftdetect`.

![syntax highlighting](https://imgur.com/X8KVpHP.png)
