#!/bin/sh -e

cd "$(dirname "$0")"

ECUKES_EMACS=${EMACS:-$(which emacs)}

echo "*** Emacs version ***"
echo "ECUKES_EMACS = $ECUKES_EMACS"
"$ECUKES_EMACS" --version
echo

"$ECUKES_EMACS" --batch --eval "(unless (ignore-errors (require 'cl-lib)) (package-refresh-contents) (package-install 'cl-lib))"

cask exec ecukes

if [ -n "$TRAVIS_COMMIT_RANGE" ]; then
    CHANGED_RECIPES=$(git show --pretty=format: --name-only "$TRAVIS_COMMIT_RANGE" |grep -e '^recipes/')
    if [ -n "$CHANGED_RECIPES" ]; then
        for recipe_file in $CHANGED_RECIPES; do
            recipe_name=$(echo "$recipe_file"|sed 's/^recipes\///')
            echo "Building new/modified recipe: $recipe_name"
            "$ECUKES_EMACS" --batch --eval "(progn (load-file \"package-build.el\")(package-build-archive '$recipe_name))"
        done
    fi
fi
