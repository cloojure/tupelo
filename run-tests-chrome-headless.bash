#!/bin/bash -v

rm -rf target
clojure -m figwheel.main -co tests.cljs.edn  -m tst.runner

