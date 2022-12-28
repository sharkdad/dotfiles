#!/bin/bash

cd "$(dirname "$0")"

for f in $(cat dotfiles); do
    [[ ! -f "$f" ]] && echo "bad file: $f" && exit 1

    homefile="$HOME/$f"
    [[ -f "$homefile" ]] && mv "$homefile" "$f"
    [[ -e "$homefile" ]] && [[ ! -h "$homefile" ]] && echo "check $homefile"
    
    ln -srfn "$f" "$homefile"
done
