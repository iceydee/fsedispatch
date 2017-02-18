#!/bin/bash

TARGET=${1-"airports"}
SCRAPE_LIST=`cat ${FSE_SCRAPE_LIST}`
DEBUG=${2}

for LINE in ${SCRAPE_LIST}; do
  echo "Fetching ${LINE}"
  if [ "${DEBUG}" == "--debug" ]; then
  	FSE_ICAO_FILE="${LINE}" phantomjs --load-images=false ./src/${TARGET}.js
  else
  	FSE_ICAO_FILE="${LINE}" phantomjs --load-images=false ./src/${TARGET}.js > /dev/null
  fi
  
done
