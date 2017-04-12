library("geosphere")
library("measurements")

icaodata <<- read.csv("./data/icaodata.csv", header=TRUE, sep=",")
icaoloc <<- icaodata[,c("icao", "lon", "lat")]
colnames(icaoloc) <- c("Location", "Longitude", "Latitude")

waterloc <<- icaodata[icaodata$type == "water",c("icao", "lon", "lat")]
colnames(waterloc) <- c("Location", "Longitude", "Latitude")

icao.get <- function(icao) {
  return(icaodata[icaodata$icao==icao,])
}
icao.filterWater <- function(locationDF) {
  a <- locationDF[!(locationDF$Location %in% waterloc$Location),]
  return (a)
}
icao.location <- function(icao) {
  return (as.numeric(icaoloc[icaoloc$Location==icao,c("Longitude", "Latitude")]))
}

if (file.exists("./data/distance_cache.csv")) {
  distanceCache <<- read.csv("./data/distance_cache.csv")
} else {
  distanceCache <<- data.frame(start = character(), end = character(), distance = integer(), stringsAsFactors = FALSE)
}

icao.cacheData <- function(startNo, endNo) {
  a <- as.character(icaodata$icao[startNo:endNo])
  b <- expand.grid(a, a)
  x <- as.character(b[,1])
  y <- as.character(b[,2])
  sapply(1:length(x), function(n) {icao.distance(x[n], y[n])})
  icao.writeDistanceCacheToDisk()
}

icao.distanceForPair <- function(pair) {
  a <- distanceCache[distanceCache$start == pair[1] & distanceCache$end == pair[2],]
  if (nrow(a) < 1) {
    return (NULL)
  }
  return (a[1,c("distance")])
}

icao.cacheDistance <- function(pair, distance) {
  distanceCache[nrow(distanceCache)+1,] <<- list(toString(pair[1]), toString(pair[2]), as.integer(distance))
  icao.writeDistanceCacheToDisk()
}

icao.writeDistanceCacheToDisk <- function() {
  write.csv(distanceCache, file = "./data/distance_cache.csv")
}

distFactor <<- 44.68331

crudeDistance <- function(loc1, loc2) {
  x <- loc1[2] - loc2[2]
  y <- (loc1[1] - loc2[1]) * cos(loc2[2])
  z <- distFactor * sqrt(x * x + y * y)
  if (z < 300) {
    return (z * 0.75)
  } else {
    return (z * 1.35)
  }
}

icao.distance <- function(start, end) {
  if (start == end) {
    return (0)
  }
  
  # Make sure we don't check same pair twice
  if (start > end) {
    p <- c(end, start)
  } else {
    p <- c(start, end)
  }
  
  loc1 <- icao.location(p[1])
  loc2 <- icao.location(p[2])
    
  distance <- round(conv_unit(distVincentyEllipsoid(loc1, loc2), "m", "naut_mi"))

  return (distance)
}

filterLatLon <- function(loc, maxDistance = 50) {
  # First diff the lon
  curDistance <- 0
  diffLon <- 0
  while (curDistance < maxDistance) {
    diffLon <- diffLon + 1
    checkLoc <- loc
    checkLoc[1] <- checkLoc[1] + diffLon
    curDistance <- crudeDistance(loc, checkLoc)
  }
  
  # Then diff the lat
  curDistance <- 0
  diffLat <- 0
  while (curDistance < maxDistance) {
    diffLat <- diffLat + 1
    checkLoc <- loc
    checkLoc[2] <- checkLoc[2] + diffLat
    curDistance <- crudeDistance(loc, checkLoc)
  }
  
  return (c(diffLon, diffLat))
}

icao.nearby <- function(start, maxDistance = 50, minSize = 1500) {
  startLoc <- icao.location(start)
  filterLoc <- filterLatLon(startLoc, maxDistance)
  lonRange <- c(startLoc[1] - filterLoc[1], startLoc[1] + filterLoc[1])
  latRange <- c(startLoc[2] - filterLoc[2], startLoc[2] + filterLoc[2])
  
  x <- icaodata[icaodata$lon > lonRange[1] & icaodata$lon < lonRange[2] & icaodata$lat > latRange[1] & icaodata$lat < latRange[2],]
  x <- x[x$size >= minSize,]
  x$icao <- as.character(x$icao)
  x$distance <- sapply(x$icao, icao.distance, end = start)
  x <- x[x$distance <= maxDistance,]
  x <- x[x$icao != start,]
  x <- x[!is.na(x$icao),]
  return (x[order(x$distance),])
}
