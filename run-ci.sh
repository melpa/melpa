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
    if [ -f "./recipes/${recipe_name}" ]; then
        echo "----------------------------------------------------"
        echo "Building new/modified recipe: ${recipe_name}"
        ~/.cask/bin/cask emacs -Q --batch -l package-build \
			 --eval "(dolist (what (split-string \"working archive recipes\")) (custom-set-default (intern (format \"package-build-%s-dir\" what)) (expand-file-name what default-directory)))" \
			 --eval "(package-build-archive \"${recipe_name}\")"
    fi
done

echo "Build successful"
