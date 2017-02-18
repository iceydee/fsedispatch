#!/bin/bash

echo ${PATH}}
echo `which phantomjs`

TARGET=${1-"airports"}

/c/Users/mio/Documents/Dev/phantomjs/bin/phantomjs.exe --load-images=false ./src/${TARGET}.js
