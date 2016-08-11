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

echo "Building recipes touched in commits $TRAVIS_COMMIT_RANGE"
changed_recipes=$(./travis-changed-files|grep -e '^recipes/[a-z0-9]'|sed 's/^recipes\///')
for recipe_name in $changed_recipes; do
    if [ -f "./recipes/$recipe_name" ]; then
        echo "----------------------------------------------------"
        echo "Building new/modified recipe: $recipe_name"
        "$ECUKES_EMACS" --batch --eval "(progn (load-file \"package-build/package-build.el\")(package-build-archive '$recipe_name))"
    fi
done

echo "Build successful"
