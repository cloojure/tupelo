#!/bin/bash
set -v
rm -rf out public/js
time (clojure -M:shadow-cljs compile test-node)
