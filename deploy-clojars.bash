#!/bin/bash 
env  \
  CLOJARS_USERNAME=cloojure  \
  CLOJARS_PASSWORD=`cat ~/alan/clojars.txt`  \
  clj -T:build deploy

