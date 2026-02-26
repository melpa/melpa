#!/bin/bash

set -eo pipefail

exec 2>&1

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
        emacs --batch --eval "(let ((debug-on-error t)) (add-to-list 'load-path \"$PWD/package-build/\")(load-file \"package-build/package-build.el\")(package-build-archive \"$recipe_name\"))"
    else
        echo "File or directory unbuildable: ./recipes/${recipe_name}"
    fi
done

# if the tooling in ./package-build changed test a couple 'interesting' recipes:
changed_tooling=$(echo "$CHANGED_FILES" | (grep -Po '(?<=^package-build/)[a-z0-9].*' || true))
if [ -n "$changed_tooling" ]; then
    for recipe_name in "evil" "kanban" "magit"; do
        echo "----------------------------------------------------"
        echo "Building recipe to test build tooling: $recipe_name"
        emacs --batch --eval "(let ((debug-on-error t)) (add-to-list 'load-path \"$PWD/package-build/\")(load-file \"package-build/package-build.el\")(package-build-archive \"$recipe_name\"))"
    done
fi

echo "Build successful"
