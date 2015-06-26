#!/bin/zsh

# Controlling values
readonly ghPagesDir=~/gh-pages          # must be absolute pathname
readonly projName="tupelo"

srcDir=$(pwd)/doc                       # absolute pathname of source
destDir=${ghPagesDir}/${projName}       # absolute pathname of destination

# Copy files
rm -rf $destDir
echo "Copying:  $srcDir  ->  $destDir"
cp -pr          $srcDir      $destDir

# Commit changes
pushd ${ghPagesDir}
  git add $projName
  git commit --all -m"Update docs for $projName"
  git push --set-upstream
popd
