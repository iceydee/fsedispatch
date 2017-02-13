source("./src/xmlHandling.R")
source("./src/icao.R")

fse.setUserKey <- function(key) {
  userkey <<- key
}

fse.setUserKey(Sys.getenv("USERKEY"))

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
  a <- a[a$NeedsRepair == 0,]
  return (a[order(a$RentalDry, a$RentalWet),])
}

fse.groupHasICAOPair <- function(groupedAssignments, start, end) {
  if (length(groupedAssignments) < 1) {
    return (FALSE)
  }
  for (n in 1:length(groupedAssignments)) {
    a <- groupedAssignments[[n]]
    if (a$FromIcao == start && a$ToIcao == end) {
      return (TRUE)
    }
  }
  return (FALSE)
}

fse.groupAssignments <- function(assignments, maxSeats = 9) {
  groupedAssignments <- list()
  for (n in 1:nrow(assignments)) {
    a <- assignments[n,]
    if (fse.groupHasICAOPair(groupedAssignments, a$FromIcao, a$ToIcao)) {
      next
    }
    if (a$Amount < maxSeats) {
      i <- (length(groupedAssignments) + 1)
      x <- assignments[assignments$FromIcao == a$FromIcao & assignments$ToIcao == a$ToIcao,]
      while (sum(x$Amount) > maxSeats) {
        x <- x[1:(nrow(x)-1),]
      }
      groupedAssignments[[i]] <- x
    }
  }
  return (groupedAssignments)
}

fse.getAssignments <- function(icaos, minDistance = 0, maxDistance = 400, unittype = "passengers", maxSeats = 9, grouped = TRUE) {
  maxFetch <- 10
  if(length(icaos) > maxFetch) {
    len <- length(icaos)
    cat(sprintf("Bulk fetching %i airports assignments at a time.\n", len))
    n <- 1
    if (grouped) {
      assignments <- list()
    } else {
      assignments <- data.frame()
    }
    while (n <= len) {
      last <- (n + maxFetch - 1)
      if (last > len) {
        last <- len
      }
      cat(sprintf("Fetching icaos %i:%i - %s\n", n, last, paste(icaos[n:last], collapse = "-")))
      a <- fse.getAssignments(icaos[n:last], minDistance, maxDistance, unittype, maxSeats, grouped)
      
      if (grouped) {
        assignments <- append(assignments, a)
      } else {
        assignments <- rbind(assignments, a)
      }
      
      n <- (n + maxFetch)
    }
    return (assignments)
  }
  
  icaoList <- paste(icaos, collapse = '-')
  url <- fse.query("icao", list(search = "jobsfrom", icaos = icaoList))
  a <- fetchXML(url, paste("jobsfrom", icaoList, sep = '-'), 15)
  a <- clean(a, c(c("int"), rep("char", 3), c("int"), rep("char", 2), c("int"), rep("char", 5)))
  a$Distance <- sapply(1:nrow(a), function(n) {icao.distance(a$FromIcao[n], a$ToIcao[n])})
  a <- a[a$Distance >= minDistance & a$Distance <= maxDistance,]
  a <- a[a$UnitType == unittype,]
  a <- a[a$Amount <= maxSeats,]
  a <- a[order(-a$Pay),]
  
  if (grouped) {
    return (fse.groupAssignments(a, maxSeats = maxSeats))
  }
  return (a)
}

fse.icaoHasFBO <- function(icao) {
  url <- fse.query("icao", list(search = "fbo", icao = icao))
  x <- fetchXML(url, sprintf("fbos-%s", icao), 60 * 24 * 30, getRawXML = TRUE) # Cache for 30 days
  fbos <- as.integer(xmlGetAttr(xmlRoot(x), "total"))
  return (fbos > 0)
}

fse.query <- function(query, args) {
  baseURL <- "http://server.fseconomy.net/data"
  return (sprintf("%s?userkey=%s&format=xml&query=%s&%s", baseURL, userkey, query, fse.argsToParams(args)))
}

fse.argsToParams <- function(args) {
  return (paste(names(args), args, sep = '=', collapse = '&'))
}
