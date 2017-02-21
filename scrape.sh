#!/bin/bash

TARGET=${1-"airports"}
DEBUG=${2}

if [ "${TARGET}" == "airports" ]; then
  SCRAPE_LIST=`cat ${FSE_SCRAPE_LIST}`
  for LINE in ${SCRAPE_LIST}; do
    echo "Fetching ${LINE}"
    if [ "${DEBUG}" == "--debug" ]; then
      FSE_ICAO_FILE="${LINE}" phantomjs --load-images=false ./src/${TARGET}.js
    else
      FSE_ICAO_FILE="${LINE}" phantomjs --load-images=false ./src/${TARGET}.js > /dev/null
    fi
  done
else
  if [ "${DEBUG}" == "--debug" ]; then
    phantomjs --load-images=false ./src/${TARGET}.js
  else
    phantomjs --load-images=false ./src/${TARGET}.js > /dev/null
  fi
fi
