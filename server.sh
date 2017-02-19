#!/bin/bash

. ./.envrc

PORT=${PORT-8000}

Rscript -e "library(shiny); shiny::runApp(port = ${PORT})"
