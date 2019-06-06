#!/bin/bash -v

rm -rf target
time clojure  -m figwheel.main  --compile-opts tests.cljs.edn  -m tst.minerva.test-runner

