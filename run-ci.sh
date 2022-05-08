#!/bin/bash

set -eo pipefail

exec 2>&1
cd "$(dirname "$0")"

EMACS=${EMACS:-$(command -v emacs)}

echo "*** Emacs version ***"
echo "EMACS = $EMACS"
"$EMACS" --version
echo

changed_recipes=$(echo "$CHANGED_FILES" | (grep -Po '(?<=^recipes/)[a-z0-9].*' || true))
for recipe_name in $changed_recipes; do
    if [ -f "./recipes/$recipe_name" ]; then
        echo "----------------------------------------------------"
        echo "Building new/modified recipe: $recipe_name"
        emacs --batch --eval "(progn (add-to-list 'load-path \"$PWD/package-build/\")(load-file \"package-build/package-build.el\")(package-build-archive \"$recipe_name\"))"
    fi
done

echo "Build successful"
