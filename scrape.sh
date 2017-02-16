#!/bin/bash

TARGET=${1-"airports"}

phantomjs --load-images=false ./src/${TARGET}.js
