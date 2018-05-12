#!/bin/bash

file="$1"
if [[ ! -f "$file" ]] ; then
    echo 'File "'$file'" does not exist.'
    exit
fi

if [ ! -z "$2" -a "$2" != " " ]; then
  destfile="$2/$(basename -- "$file")"
else
  destfile=$(basename -- "$file")
fi
interp="${file##*.}"

# generate symbolic link
$(ln -sf "$file" "$destfile")

# generate test data
$(stack exec "$interp" -- "$file" > "$destfile.out")
$(stack exec "$interp" -- -j "$file" > "$destfile.json.out")

$(stack exec "$interp" -- invert "$file" > "$destfile.invert.out")
$(stack exec "$interp" -- invert -j "$file" > "$destfile.json.invert.out")

$(stack exec "$interp" -- translate "$file" > "$destfile.translate.out")
$(stack exec "$interp" -- translate -j "$file" > "$destfile.json.translate.out")

$(stack exec "$interp" -- typeof "$file" > "$destfile.typeof.out")
$(stack exec "$interp" -- typeof -j "$file" > "$destfile.json.typeof.out")
