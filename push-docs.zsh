#!/bin/zsh

# Controlling values
readonly ghPagesDir=~/gh-pages          # must be absolute pathname
readonly projName="cooljure"

srcDir=$(pwd)/doc                       # absolute pathname of source
destDir=${ghPagesDir}/${projName}       # absolute pathname of destination

rm -rf   $destDir
cp -pr   $docsDir  $destDir
pushd ${ghPagesDir}
  git add $projName
  git commit --all -m"Update docs for $projName"
  git push --set-upstream
popd
