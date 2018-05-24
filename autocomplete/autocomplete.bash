function _autocomplete_srl_or_rl() {
  if [[ $# != 3 ]]; then
    return;
  fi
  local lng="$1"
  local allmodes="run translate invert typeof"
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
