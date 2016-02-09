#!/bin/sh -e

exec 2>&1
cd "$(dirname "$0")"

ECUKES_EMACS=${EMACS:-$(which emacs)}

echo "*** Emacs version ***"
echo "ECUKES_EMACS = $ECUKES_EMACS"
"$ECUKES_EMACS" --version
echo

"$ECUKES_EMACS" --batch --eval "(unless (ignore-errors (require 'cl-lib)) (package-refresh-contents) (package-install 'cl-lib))"

cask exec ecukes

if [ -n "$TRAVIS_COMMIT_RANGE" ]; then
    echo "Building recipes touched in commits $TRAVIS_COMMIT_RANGE"
    changed_recipes=$(git show --pretty=format: --name-only "$TRAVIS_COMMIT_RANGE" |grep -e '^recipes/[a-z0-9]'|sed 's/^recipes\///'|uniq)
    for recipe_name in $changed_recipes; do
        if [ -f "./recipes/$recipe_name" ]; then
            echo "----------------------------------------------------"
            echo "Building new/modified recipe: $recipe_name"
            "$ECUKES_EMACS" --batch --eval "(progn (load-file \"lisp/package-build.el\")(package-build-archive '$recipe_name))"
        fi
    done
fi

echo "Build successful"
