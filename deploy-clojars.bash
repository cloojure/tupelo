#!/bin/bash
set -v

env  \
  CLOJARS_USERNAME=cloojure  \
  CLOJARS_PASSWORD=`cat ~/alan/clojars.txt`  \
  clj  -T:build  tupelo.deps-build-deploy/deploy-clojars
