#!/bin/bash -v

rm -rf target
clojure  -m figwheel.main  --build dev  --repl   # or -b, -r

