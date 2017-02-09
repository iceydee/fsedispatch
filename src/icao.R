library("geosphere")
library("measurements")

icaodata <<- read.csv("./data/icaodata.csv", header=TRUE, sep=",")
icaoloc <<- icaodata[,c("icao", "lon", "lat")]
colnames(icaoloc) <- c("Location", "Longitude", "Latitude")

icao.get <- function(icao) {
  return(icaodata[icaodata$icao==icao,])
}
icao.location <- function(icao) {
  return (as.numeric(icaoloc[icaoloc$Location==icao,c("Longitude", "Latitude")]))
}
icao.distance <- function(start, end) {
  # Make sure we don't check same pair twice
  if (start < end) {
    loc1 <- icao.location(start)
    loc2 <- icao.location(end)
  } else {
    loc1 <- icao.location(end)
    loc2 <- icao.location(start)
  }
  
  # TODO: Add caching
  
  return (round(conv_unit(distVincentyEllipsoid(loc1, loc2), "m", "naut_mi")))
}
icao.nearby <- function(start, maxDistance = 50) {
  x <- icaodata
  y$distance <- sapply(as.character(y$icao), icao.distance, end = start)
  return (y[y$distance <= maxDistance,])
}
