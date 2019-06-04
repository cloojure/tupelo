#!/bin/bash
echo "********** headless chrome - enter **********"
/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome   --headless  --disable-gpu  --repl "$@"
echo "********** headless chrome - leave **********"

