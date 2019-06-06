#!/bin/bash -v
# npm install phantomjs    # does not support current React
# npm install slimerjs     # does not support current Firefox
npm install karma karma-cljs-test  --save-dev
npm install karma-junit-reporter   --save-dev
npm install karma-chrome-launcher  karma-firefox-launcher  karma-safari-launcher  --save-dev
