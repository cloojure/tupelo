#!/bin/bash -v

rm -rf target
clojure -m figwheel.main --build-once dev  # or -bo

