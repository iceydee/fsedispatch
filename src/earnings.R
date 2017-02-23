source("./src/fseconomy.R")

fuelWeight <- function(vol, type = 0) {
  if (type == 0) { # 100LL
    return (vol * 2.72)
  }
  return (vol * 3.08)
}

passengerWeight <- function(passengers) {
  return (passengers * 73)
}

maxPayload <- function(aircraft) {
  return (aircraft$MTOW - aircraft$EmptyWeight)
}

maxCargo <- function(aircraft, volFuel, passengers = 0) {
  return (maxPayload(aircraft) - fuelWeight(volFuel, aircraft$FuelType) - passengerWeight(passengers))
}

maxPassengers <- function(aircraft, volFuel, cargo = 0) {
  return (maxPayload(aircraft) - fuelWeight(volFuel, aircraft$FuelType) - cargo)
}

is.dry <- function(assignment) {
  if (is.na(assignment$DryEarnings) && is.na(assignment$WetEarnings)) {
    return (NA)
  }
  if (is.na(assignment$DryEarnings)) {
    return (F)
  }
  if (is.na(assignment$WetEarnings)) {
    return (T)
  }
  if (assignment$DryEarnings > assignment$WetEarnings) {
    return (T)
  }
  
  return (F)
}

gatherResults <- function(leg1, leg2, maxDistance) {
  results <- data.frame(
    start = character(),
    mid = character(),
    end = character(),
    amount1 = integer(),
    commodity1 = character(),
    earnings1 = integer(),
    dry1 = logical(),
    assignmentIds1 = character(),
    amount2 = integer(),
    commodity2 = character(),
    earnings2 = integer(),
    dry2 = logical(),
    assignmentIds2 = character(),
    totalEarnings = integer(),
    distance1 = integer(),
    fuelUsage1 = integer(),
    duration1 = integer(),
    distance2 = integer(),
    fuelUsage2 = integer(),
    duration2 = integer(),
    totalDistance = integer(),
    totalDuration = integer(),
    stringsAsFactors = FALSE)
  for (n in 1:nrow(leg1)) {
    a <- leg1[n,]
    b <- leg2[leg2$FromIcao == a$ToIcao,]
    maxBDistance <- maxDistance - a$Distance
    b <- b[b$Distance < maxBDistance,]
    b <- b[order(-b$Earnings),]
    b <- b[1,]
    
    if (nrow(b) > 0) {
      results[n,] <- list(
        start = a$FromIcao,
        mid = a$ToIcao,
        end = b$ToIcao,
        amount1 = a$Amount,
        commodity1 = a$Commodity,
        earnings1 = a$Earnings,
        dry1 = is.dry(a),
        assignmentIds1 = a$AssignmentIds,
        amount2 = b$Amount,
        commodity2 = b$Commodity,
        earnings2 = b$Earnings,
        dry2 = is.dry(b),
        assignmentIds2 = b$AssignmentIds,
        totalEarnings = (a$Earnings + b$Earnings),
        distance1 = a$Distance,
        fuelUsage1 = a$FuelUsage,
        duration1 = a$Duration,
        distance2 = b$Distance,
        fuelUsage2 = b$FuelUsage,
        duration2 = b$Duration,
        totalDistance = a$Distance + b$Distance,
        totalDuration = a$Duration + b$Duration
      )
    } else {
      results[n,] <- list(
        start = a$FromIcao,
        mid = NA,
        end = a$ToIcao,
        amount1 = a$Amount,
        commodity1 = a$Commodity,
        earnings1 = a$Earnings,
        dry1 = is.dry(a),
        assignmentIds1 = a$AssignmentIds,
        amount2 = NA,
        commidity2 = NA,
        earnings2 = NA,
        dry2 = NA,
        assignmentIds2 = NA,
        totalEarnings = a$Earnings,
        distance1 = a$Distance,
        fuelUsage1 = a$FuelUsage,
        duration1 = a$Duration,
        distance2 = NA,
        fuelUsage2 = NA,
        duration2 = NA,
        totalDistance = a$Distance,
        totalDuration = a$Duration
      )
    }
  }
  
  results <- results[order(-results$totalEarnings),]
  return (as.data.frame(results))
}

findNearestAircraft <- function(assignments, searchICAO, matchICAO) {
  if (nrow(assignments) < 1) {
    return (assignments)
  }
  for (n in 1:nrow(assignments)) {
    matchRow <- match(assignments$FromIcao[n], searchICAO, nomatch = NULL)
    if (is.null(matchRow) || is.na(matchRow)) {
      cat(sprintf("%s not found in searchICAO - %s\n", assignments$FromIcao[n], paste(searchICAO, collapse = "-")))
      assignments$Location[n] <- matchICAO[1]
      next
    }
    newLoc <- matchICAO[matchRow]
    if (is.null(newLoc) || is.na(newLoc)) {
      cat(sprintf("%s == NULL!!\n", assignments$FromIcao[n]))
      assignments$Location[n] <- matchICAO[1]
      next
    }
    assignments$Location[n] <- newLoc
  }
  return (assignments)
}

getRankedAssignments <- function(rentalAircraft, minDistance = 50, maxDistance = 400, searchICAO = NULL, matchICAO = NULL, progress = function(a, b) {}) {
  aircraft <- fse.getAircraft(rentalAircraft$MakeModel[1])
  
  findNearestAC <- FALSE
  
  if (is.null(searchICAO) || is.na(searchICAO)) {
    searchICAO <- rentalAircraft$Location
  } else {
    findNearestAC <- TRUE
  }
  
  # Find assignments
  maxSeats <- (aircraft$Seats - 1) # TODO: Use fuel calc for this
  assignments <- fse.getAssignments(
    searchICAO,
    minDistance = minDistance, maxDistance = maxDistance,
    maxSeats = maxSeats,
    grouped = TRUE,
    progress = progress
  )
  
  if (length(assignments) < 1) {
    return (data.frame())
  }
  
  groupedAssignments <- assignments[[1]][0,]
  groupedAssignments["PtCount"] <- integer(0)
  groupedAssignments["AssignmentIds"] <- character(0)
  
  for (n in 1:length(assignments)) {
    a <- assignments[[n]]
    b <- a[1,]
    b$Id <- n
    b$Amount <- sum(a$Amount)
    b$Pay <- sum(a$Pay)
    b$PtCount <- nrow(a[a$PtAssignment == "true",])
    b$AssignmentIds <- paste(a$Id, collapse = ",")
    groupedAssignments[n,] <- b
  }
  groupedAssignments <- groupedAssignments[order(-groupedAssignments$Pay),]
  
  if (aircraft$FuelType == 0) {
    fuelType <- "100LL"
  } else {
    fuelType <- "Jet-A"
  }
  groupedAssignments$FuelPrice <- sapply(1:nrow(groupedAssignments), function(n) {
    fse.fuelPrice(groupedAssignments$FromIcao[n], fuelType)
  })
  
  if (findNearestAC) {
    groupedAssignments <- findNearestAircraft(groupedAssignments, searchICAO, matchICAO)
  }
  
  groupedAssignments <- calc.assignments(rentalAircraft, groupedAssignments)
  groupedAssignments <- groupedAssignments[order(-groupedAssignments$Earnings),]
  
  return (groupedAssignments)
}

calc.assignments <- function(rentalAircraft, assignments) {
  aircraft <- fse.getAircraft(rentalAircraft$MakeModel[1])
  fse.fetchAirports(c(assignments$FromIcao, assignments$ToIcao))
  assignments$GroundCrewFee <- sapply(1:nrow(assignments), function(n) {calc.groundCrewFee(assignments[n,])})
  assignments$BookingFee <- sapply(1:nrow(assignments), function(n) {calc.bookingFee(assignments[n,])})
  assignments$FuelUsage <- sapply(1:nrow(assignments), function(n) {calc.fuelUsage(aircraft, assignments$Distance[n])})
  assignments$Duration <- sapply(1:nrow(assignments), function(n) {calc.duration(aircraft, assignments$Distance[n])})
  assignments$DistanceBonus <- sapply(1:nrow(assignments), function(n) {
    ac <- rentalAircraft[rentalAircraft$Location == assignments$Location[n],]
    if (nrow(ac) < 1) {
      cat(sprintf("No rental aircraft found for %s: %s\n", assignments$Location[n], paste(rentalAircraft$Location, collapse = "-")))
      return (0)
    }
    calc.distanceBonus(ac[1,], assignments[n,])
  })
  assignments$DryEarnings <- sapply(1:nrow(assignments), function(n) {
    ac <- rentalAircraft[rentalAircraft$Location == assignments$Location[n],]
    ac <- ac[ac$RentalDry > 0,]
    ac <- ac[order(ac$RentalDry),]
    if (nrow(ac) < 1) {
      cat(sprintf("No rental aircraft found for %s: %s\n", assignments$Location[n], paste(rentalAircraft$Location, collapse = "-")))
      return (0.0)
    }
    if (ac$RentalDry == 0) {
      return (0.0)
    }
    calc.earnings(ac[1,], aircraft, assignments[n,])
  })
  assignments$WetEarnings <- sapply(1:nrow(assignments), function(n) {
    ac <- rentalAircraft[rentalAircraft$Location == assignments$Location[n],]
    ac <- ac[ac$RentalWet > 0,]
    ac <- ac[order(ac$RentalWet),]
    if (nrow(ac) < 1) {
      cat(sprintf("No rental aircraft found for %s: %s\n", assignments$Location[n], paste(rentalAircraft$Location, collapse = "-")))
      return (0.0)
    }
    calc.earnings(ac[1,], aircraft, assignments[n,], dry = FALSE)
  })
  assignments$Earnings <- sapply(1:nrow(assignments), function(n) {max(assignments$DryEarnings[n], assignments$WetEarnings[n])})
  return (assignments)
}

calc.earnings <- function(rentalAircraft, aircraft, assignment, dry = TRUE) {
  if (rentalAircraft$RentalWet == 0) {
    dry <- TRUE
  }
  if (rentalAircraft$RentalDry == 0) {
    dry <- FALSE
  }
  
  cost <- 0
  if (dry) {
    fuelCost <- (calc.fuelUsage(aircraft, assignment$Distance) * assignment$FuelPrice)
    cost <- ((rentalAircraft$RentalDry * assignment$Duration) + fuelCost)
  } else {
    cost <- (rentalAircraft$RentalWet * assignment$Duration)
  }

  return (assignment$Pay - cost + assignment$DistanceBonus - assignment$GroundCrewFee - assignment$BookingFee)
}

calc.groundCrewFee <- function(assignment) {
  groundCrewFee <- 0.00
  baseFee <- assignment$Pay * 0.05
  
  if (fse.icaoHasFBO(assignment$FromIcao)) {
    groundCrewFee <- groundCrewFee + baseFee
  }
  if (fse.icaoHasFBO(assignment$ToIcao)) {
    groundCrewFee <- groundCrewFee + baseFee
  }
  
  return (groundCrewFee)
}

calc.bookingFee <- function(assignment) {
  if (assignment$PtCount > 5) {
    return (assignment$Pay * (assignment$PtCount / 100))
  }
  return (0.00)
}

calc.distanceBonus <- function(rentalAircraft, assignment) {
  if (is.na(assignment$FromIcao) ||
      is.na(assignment$ToIcao) ||
      is.na(rentalAircraft$Home)) {
    return (0.00)
  }
  oldHomeDistance <- icao.distance(assignment$FromIcao, rentalAircraft$Home)
  newHomeDistance <- icao.distance(assignment$ToIcao, rentalAircraft$Home)
  distanceDiff <- oldHomeDistance - newHomeDistance
  return (rentalAircraft$Bonus * distanceDiff / 100)
}

calc.fuelUsage <- function(aircraft, distance) {
  return (aircraft$GPH * calc.duration(aircraft, distance))
}

calc.duration <- function(aircraft, distance) {
  # TODO: Include taxi, climb, descent
  return (as.integer(distance) / as.integer(aircraft$CruiseSpeed))
}
