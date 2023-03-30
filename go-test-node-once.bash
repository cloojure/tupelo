#!/bin/bash
set -v
rm -rf out public/js

clojure -M:shadow-cljs compile test-node
