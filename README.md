# BscProject


# CLI

## Build

## Autocompletion BASH

    function _rl() {
      local allmodes="run translate invert typeof"
      local prev="${COMP_WORDS[COMP_CWORD - 1]}"
      local cur="${COMP_WORDS[COMP_CWORD]}"
      local files=( $(IFS=' '; find . -maxdepth 1 -name "${cur}*.rl") )
      if [[ "$prev"  = "rl" ]]; then
        local modes=( $(compgen -W "$allmodes" -- "$cur") )
        COMPREPLY=( ${modes[@]} ${files[@]/.\//} )
      elif [[ "$allmodes" =~ "$prev" ]]; then
        COMPREPLY=( ${files[@]/.\//} );
      fi
    }
    complete -F _rl rl

    function _srl() {
      local allmodes="run translate invert typeof"
      local prev="${COMP_WORDS[COMP_CWORD - 1]}"
      local cur="${COMP_WORDS[COMP_CWORD]}"
      local files=( $(IFS=' '; find . -maxdepth 1 -name "${cur}*.srl") )
      if [[ "$prev"  = "srl" ]]; then
        local modes=( $(compgen -W "$allmodes" -- "$cur") )
        COMPREPLY=( ${modes[@]} ${files[@]/.\//} )
      elif [[ "$allmodes" =~ "$prev" ]]; then
        COMPREPLY=( ${files[@]/.\//} );
      fi
    }
    complete -F _srl srl


# Web Interface
