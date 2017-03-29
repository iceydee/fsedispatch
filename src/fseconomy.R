source("./src/xmlHandling.R")
source("./src/icao.R")
source("./src/scrape.R")

passengerWeight <- function(passengers) {
  return (passengers * 73)
}

fse.setUserKey <- function(key) {
  userkey <<- key
}

fse.setServiceKey <- function(key) {
  servicekey <<- key
}

fse.setUserKey(Sys.getenv("USERKEY"))
fse.setServiceKey(Sys.getenv("SERVICEKEY"))

fse.getAircraft <- function(makeModel = NULL) {
  url <- fse.query("aircraft", list(search = "configs"))
  a <- fetchXML(url, "aircraft", maxAge = (60 * 24 * 7))
  a <- clean(a, c(c("char"), rep("int", 23)))
  if (! is.null(makeModel)) {
    return (a[a$MakeModel == makeModel,])
  }
  return (a)
}

fse.findAircraft <- function(makeModel, waterOk = TRUE) {
  url <- fse.query("aircraft", list(search = "makemodel", makemodel = URLencode(makeModel)))
  a <- fetchXML(url, paste(makeModel, "-search", sep = '', collapse = ''), 15)
  a <- clean(a, c(c("int"), rep("char", 6), rep("int", 2), c("char"), rep("int", 2), c("char"), rep("int", 2), c("char", "double", "int"), rep("char", 4), rep("double", 2)))
  a <- merge(a, icaoloc, by = "Location")
  if (!waterOk) {
    a <- icao.filterWater(a)
  }
  return (a)
}

fse.findRentalAircraft <- function(makeModel, waterOk = TRUE) {
  a <- fse.findAircraft(makeModel, waterOk = waterOk)
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

fse.groupAssignments <- function(assignments, maxSeats = 9, maxCargo = 1000) {
  groupedAssignments <- list()
  if (nrow(assignments) < 1) {
    return (groupedAssignments)
  }
  for (n in 1:nrow(assignments)) {
    a <- assignments[n,]
    if (fse.groupHasICAOPair(groupedAssignments, a$FromIcao, a$ToIcao)) {
      next
    }
    
    if (a$Weight < maxCargo) {
      i <- (length(groupedAssignments) + 1)
      x <- assignments[assignments$FromIcao == a$FromIcao & assignments$ToIcao == a$ToIcao,]
      while (sum(x$Weight) > maxCargo || sum(x$Seats) > maxSeats) {
        x <- x[1:(nrow(x)-1),]
      }
      groupedAssignments[[i]] <- x
    }
  }
  return (groupedAssignments)
}

fse.getAssignments <- function(icaos, minDistance = 0, maxDistance = 400, maxSeats = 9, maxCargo = 1000, grouped = TRUE, progress = function(a, b) {}) {
  icaos <- unique(sort(icaos))
  maxFetch <- 100
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
      a <- fse.getAssignments(icaos[n:last], minDistance, maxDistance, maxSeats, maxCargo, grouped)
      progress(n / len, sprintf("%.0f / %.0f", n, len))
      
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
  name <- safeLongName(paste(icaoList, collapse = '-'))
  a <- fetchXML(url, paste("jobsfrom", name, sep = '-'), 15)
  a <- clean(a, c(c("int"), rep("char", 3), c("int"), rep("char", 2), c("int"), rep("char", 5)))
  a$Distance <- sapply(1:nrow(a), function(n) {icao.distance(a$FromIcao[n], a$ToIcao[n])})
  a <- a[a$Distance >= minDistance & a$Distance <= maxDistance,]
  a$Seats <- sapply(1:nrow(a), function(n) {
    if (a$UnitType[n] == "passengers") {
      return (a$Amount[n])
    }
    return (0)
  })
  a$Weight <- sapply(1:nrow(a), function(n) {
    if (a$UnitType[n] == "kg") {
      return (a$Amount[n])
    }
    return (passengerWeight(a$Amount[n]))
  })
  
  a <- a[a$Seats <= maxSeats,]
  a <- a[a$Weight <= maxCargo,]
  
  # Filter out Pilot for Hire assignments
  a <- a[!grepl("Pilot for Hire", a$Commodity),]
  
  a <- a[order(-a$Pay),]
  
  if (grouped) {
    if (nrow(a) < 1) {
      return (list())
    }
    return (fse.groupAssignments(a, maxSeats = maxSeats, maxCargo = maxCargo))
  }
  return (a)
}

fse.query <- function(query, args) {
  baseURL <- "http://server.fseconomy.net/data"
  return (sprintf("%s?servicekey=%s&format=xml&query=%s&%s", baseURL, servicekey, query, fse.argsToParams(args)))
}

fse.argsToParams <- function(args) {
  return (paste(names(args), args, sep = '=', collapse = '&'))
}
