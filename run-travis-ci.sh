#!/bin/sh -e

exec 2>&1
cd "$(dirname "$0")"

ECUKES_EMACS=${EMACS:-$(which emacs)}

echo "*** Emacs version ***"
echo "ECUKES_EMACS = $ECUKES_EMACS"
"$ECUKES_EMACS" --version
echo

cask exec ecukes

echo "Building recipes touched in commits $TRAVIS_COMMIT_RANGE"
changed_recipes=$(./travis-changed-files|grep -e '^recipes/[a-z0-9]'|sed 's/^recipes\///')
for recipe_name in $changed_recipes; do
    if [ -f "./recipes/$recipe_name" ]; then
        echo "----------------------------------------------------"
        echo "Building new/modified recipe: $recipe_name"
        cask emacs --batch --eval "(progn (add-to-list 'load-path \"$TRAVIS_BUILD_DIR/package-build/\")(load-file \"package-build/package-build.el\")(package-build-archive \"$recipe_name\"))"
    fi
done

echo "Build successful"
