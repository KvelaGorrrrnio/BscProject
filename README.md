# Problem definition

The project concerns the implementation of interpreters for each of the
two reversible programming languages RL and SRL as described in the
article <i>Fundamentals of reversible flowchart languages</i>. RL,
which is an abbreviation of Reversible Language, is a low-level,
assembler-style language with jumps that is, thus, non-structured. SRL,
which is an abbreviation of Structured Reversible Language, is, as the
name implies, a structured version of RL (with conditionals and loops).
Furthermore, our implementation will support two interesting program transformations, namely
program inversion of each language and translation <i>between</i> them - as proven possible by the Reversible Structured Program Theorem. To test our implementation and evaluate the practicality of the two
languages, we write a collection of test programs of varying complexity.

# Boundaries of problem definition

The focus of the project is on writing the two interpreters in Haskell.
Each of the interpreters has to run reasonably fast, although the main
focus will be on correctly implementing the full feature set of both
languages.
We may choose to alter the syntax or semantics of the languages in
question in cooperation with our supervisor - given that the
alteration is well argued for.
The test programs should demonstrate all of the implemented features of
both languages. Some of the test programs should reflect real world
problems, in the sense that they should have some level of purpose and
complexity.
We will also implement a command-line interface for the interpreters
along with a web-based interface for running and testing the languages.

# Motivation

Many sequential programming languages today are forward deterministic; a
given instruction in a program uniquely defines the next state. Most of
these languages, however, may discard information along the execution
path. Thus, one given instruction in a program may not uniquely define
the previous state. These languages are therefore not backward
deterministic; they are irreversible.
There are, however, reversible programming languages that do not allow
this loss of information. A given instruction still uniquely defines the
next state, but it also uniquely defines the previous. Two such
languages, proposed in the article *Fundamentals of reversible
flowchart languages*, are RL and SRL.
*Landauer's Principle* states that erased information must be
dissipated as heat. That is, the lower limit of power consumption in a
microprocessor depends, in some way, on erasure of information. Thus,
reversible computing, which does not allow loss of information, can
ideally circumvent this lower bound and improve upon the power
efficiency of modern computers.
In this project we will implement interpreters for RL and SRL along with
some useful program transformations.

# Tasks

1.  Decide on whether to use a parser generator or write our own parser.
    If we choose to use a parser generator, we have to decide on which
    one to use.

2.  Identify the syntax and semantics of core features and full
    implementation features.

3.  Implement lexers, parsers and interpreters for core features. Later,
    extend these into the full implementations.

4.  Implement program transformations, namely inversion and translation
    between the two languages.

5.  Write a small test suite and a collection of both some trivial and
    some non-trivial programs reflecting real world problems. The
    programs should test - and hopefully demonstrate - the
    usefulness of RL and SRL. Test programs should, together, make use
    of all implemented features.

6.  Implement minimal command-line- and web interfaces for interacting
    with the interpreters.

7.  Write the report.



# Getting Started

## Prerequisites

### Interpreters
To build and run the interpreters, haskell-stack is required.
```bash
brew install haskell-stack cabal-install ghc # MacOS
curl -sSL https://get.haskellstack.org/ | sh # Unix
wget -qO- https://get.haskellstack.org/ | sh # Unix alternative
```
For windows see [stack documentation](https://docs.haskellstack.org/en/stable/README/).

### Web interface
To build and run the web interface Node is required.
```bash
brew install node               # Mac OS
sudo apt-get install nodejs npm # Ubuntu
sudo pacman -S nodejs npm       # Arch
```

## Installing

### Interpreters

The interpreters can be built with
```bash
make src # Binaries can be found in /src/bin
```

To instal the binaries to local bin
```bash
make install
```

### Webinterface
The web interface can be built with
```bash
make web
```

## Usage

### Interpreters

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

### Webinterface

To start the web server, run
```bash
make server # Starts server at http://localhost:3001/
```
