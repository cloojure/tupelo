#!/bin/bash -v

rm -rf target
clojure  -m figwheel.main  -O advanced  --build-once dev     # or -bo

