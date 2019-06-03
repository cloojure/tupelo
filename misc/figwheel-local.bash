#!/bin/bash -v

rm -rf target
clojure  -m figwheel.main  --build local  --repl   # or -b, -r

