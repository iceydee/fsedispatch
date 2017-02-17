cat("FSEDispatch v0.1.0\n")
cat("------------------\n")
cat("Region fetcher\n\n")

source("./src/scrape.R")
source("./src/regions.R")
source("./src/icao.R")

readInt <- function(prompt = "") {
  cat(prompt)
  i <- file("stdin")
  a <- as.integer(readLines(i, 1))
  close(i)
  return (a)
}

printVector <- function(vector) {
  for (n in 1:length(vector)) {
    cat(sprintf("[%i] %s\n", n, as.character(vector[n])))
  }
}

printVector(regions$name)
q <- sprintf("\nWhich region? [1-%i] ", nrow(regions))
icao <- limitByRegion(icaoloc, regions[readInt(q),])
icao <- as.character(icao$Location)

fse.fetchAirports(icao)
