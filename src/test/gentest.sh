#!/bin/bash

# get source file
if [[ ! -f "$1" ]] ; then
    echo 'File "'$1'" does not exist.'
    exit
fi
file="$1"

# get destination directory
if [[ ! -d "$2" ]] ; then
    echo '"'$2'" is not a directory.'
    exit
fi
destfile="$2/$(basename -- "$file")"

# get interpreter
interp="${file##*.}"

# run normally
$(stack exec "$interp" -- "$file" > "$destfile.out")
$(stack exec "$interp" -- -j "$file" > "$destfile.json.out")

# if the output should not be an error, we want
# to have data for the other modes too
if [ "$3" != "err" ]
then
  $(stack exec "$interp" -- invert "$file" > "$destfile.invert.out")
  $(stack exec "$interp" -- invert -j "$file" > "$destfile.json.invert.out")

  $(stack exec "$interp" -- translate "$file" > "$destfile.translate.out")
  $(stack exec "$interp" -- translate -j "$file" > "$destfile.json.translate.out")
fi
