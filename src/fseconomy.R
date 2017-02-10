source("./src/xmlHandling.R")
source("./src/icao.R")

fse.setUserKey <- function(key) {
  userkey <<- key
}

cleanInt <- function(data, col) {
  data[,col] <- as.integer(as.character(data[,col]))
  return (data)
}

cleanDouble <- function(data, col) {
  data[,col] <- as.double(as.character(data[,col]))
  return (data)
}

cleanChar <- function(data, col) {
  data[,col] <- as.character(data[,col])
  return (data)
}

clean <- function(data, v) {
  for (n in 1:ncol(data)) {
    type <- v[n]
    if (type == "int") {
      data <- cleanInt(data, n)
    } else if (type == "double") {
      data <- cleanDouble(data, n)
    } else {
      data <- cleanChar(data, n)
    }
  }
  return (data)
}

fse.getAircraft <- function(makeModel = NULL) {
  url <- fse.query("aircraft", list(search = "configs"))
  a <- fetchXML(url, "aircraft", maxAge = (60 * 24 * 7))
  a <- clean(a, c(c("char"), rep("int", 23)))
  if (! is.null(makeModel)) {
    return (a[a$MakeModel == makeModel,])
  }
  return (a)
}

fse.filterLocation <- function(locationDF, lonFilter, latFilter) {
  a <- locationDF
  a <- a[a$Longitude > lonFilter[1] & a$Longitude < lonFilter[2] & a$Latitude > latFilter[1] & a$Latitude < latFilter[2],]
  return (a)
}

fse.findAircraft <- function(makeModel, lonFilter = c(-180, 180), latFilter = c(-90, 90), waterOk = TRUE) {
  url <- fse.query("aircraft", list(search = "makemodel", makemodel = URLencode(makeModel)))
  a <- fetchXML(url, paste(makeModel, "-search", sep = '', collapse = ''), 5)
  a <- clean(a, c(c("int"), rep("char", 6), rep("int", 2), c("char"), rep("int", 2), c("char"), rep("int", 2), c("char", "double", "int"), rep("char", 4), rep("double", 2)))
  a <- merge(a, icaoloc, by = "Location")
  a <- fse.filterLocation(a, lonFilter, latFilter)
  if (!waterOk) {
    a <- icao.filterWater(a)
  }
  return (a)
}

fse.findRentalAircraft <- function(makeModel, lonFilter = c(-180, 180), latFilter = c(-90, 90), waterOk = TRUE) {
  a <- fse.findAircraft(makeModel, lonFilter = lonFilter, latFilter = latFilter, waterOk = waterOk)
  a <- a[a$RentalDry > 0 | a$RentalWet > 0,]
  return (a[order(a$RentalDry, a$RentalWet),])
}

fse.getAssignments <- function(icaos, maxDistance = 400) {
  icaoList <- paste(icaos, collapse = '-')
  url <- fse.query("icao", list(search = "jobsfrom", icaos = icaoList))
  a <- fetchXML(url, paste("jobsfrom", icaoList, sep = '-'), 5)
  a <- clean(a, c(c("int"), rep("char", 3), c("int"), rep("char", 2), c("int"), rep("char", 5)))
  a$Distance <- sapply(1:nrow(a), function(n) {icao.distance(a$FromIcao[n], a$ToIcao[n])})
  a <- a[a$Distance <= maxDistance,]
  return (a[order(-a$Pay),])
}

fse.query <- function(query, args) {
  baseURL <- "http://server.fseconomy.net/data"
  return (sprintf("%s?userkey=%s&format=xml&query=%s&%s", baseURL, userkey, query, fse.argsToParams(args)))
}

fse.argsToParams <- function(args) {
  return (paste(names(args), args, sep = '=', collapse = '&'))
}
