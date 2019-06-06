#!/usr/bin/env bash
echo "********** headless chrome - enter **********"

function isMac() {
  if [[ $(uname -a) =~ "Darwin" ]]; then
    true
  else
    false
  fi
}
function isLinux() {
  if [[ $(uname -a) =~ "Linux" ]]; then
    true
  else
    false
  fi
}

if isMac ; then
  echo "found Mac"
  /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome   --headless  --disable-gpu  --repl "$@"
fi

if isLinux ; then
  echo "found Linux"
# google-chrome   --headless  --disable-gpu  --repl "$@"
  google-chrome               --disable-gpu  --repl "$@"  
fi

echo "********** headless chrome - leave **********"

