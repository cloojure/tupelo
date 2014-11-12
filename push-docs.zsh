#!/bin/zsh -v
pushd doc
git checkout gh-pages   # To be sure you're on the right branch
git add .
git commit -am "new documentation push."
git push -u origin gh-pages
popd
