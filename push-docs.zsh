#!/bin/zsh -v

ghPagesDir="~/gh-pages"
projName="cooljure"
docsDir=$(pwd)/doc

pushd $ghPagesDir
  rm -rf             $projName
  cp -pr   $docsDir  $projName
  git add            $projName
  git commit --all -m"Update docs for $projName"
  git push
popd
