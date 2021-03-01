#!/bin/bash

set -eo pipefail

exec 2>&1
cd "$(dirname "$0")"

changed_recipes=$(echo "$CHANGED_FILES" | (grep -Po '(?<=^recipes/)[a-z0-9].*' || true))
for recipe_name in $changed_recipes; do
    if [ -f "./recipes/${recipe_name}" ]; then
        echo "----------------------------------------------------"
        echo "Building new/modified recipe: ${recipe_name}"
        cask emacs --batch --eval "(progn (add-to-list 'load-path \"$PWD/package-build/\")(load-file \"package-build/package-build.el\")(package-build-archive \"${recipe_name}\"))"
        echo "Build successful: ${recipe_name}"
        if [ "${GITHUB_EVENT_NAME:-}" = "pull_request" ]; then
            echo "Linting: ${recipe_name}"
            MZDIR=~/local bash -ex scripts/melpazoid.sh ${recipe_name}
            echo "Lint successful: ${recipe_name}"
        fi
    fi
done
