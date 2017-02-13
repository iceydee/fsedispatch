source("./src/fseconomy.R")

getRankedAssignments <- function(rentalAircraft, minDistance = 50, maxDistance = 400, searchICAO = NULL) {
  aircraft <- fse.getAircraft(rentalAircraft$MakeModel[1])
  
  if (is.null(searchICAO)) {
    startLocation = NULL
    searchICAO = rentalAircraft$Location
  } else {
    if (nrow(rentalAircraft) != 1) {
      stop("Expecting just 1 rental aircraft when searching assignments on hop")
    }
    startLocation = rentalAircraft$Location
  }
  
  # Find assignments
  maxSeats <- (aircraft$Seats - 1)
  assignments <- fse.getAssignments(
    searchICAO,
    minDistance = minDistance, maxDistance = maxDistance,
    maxSeats = maxSeats,
    grouped = TRUE,
    startLocation = startLocation
  )
  
  if (length(assignments) < 1) {
    stop("No assignments matching criteria. Try changing distance.")
  }
  
  groupedAssignments <- assignments[[1]][0,]
  for (n in 1:length(assignments)) {
    a <- assignments[[n]]
    b <- a[1,]
    b$Id <- n
    b$Amount <- sum(a$Amount)
    b$Pay <- sum(a$Pay)
    groupedAssignments[n,] <- b
  }
  groupedAssignments <- groupedAssignments[order(-groupedAssignments$Pay),]
  groupedAssignments$FuelPrice <- rep(4.5, nrow(groupedAssignments))
  
  groupedAssignments <- calc.assignments(rentalAircraft, groupedAssignments)
  groupedAssignments <- groupedAssignments[order(-groupedAssignments$Earnings),]
  
  return (groupedAssignments)
}

calc.assignments <- function(rentalAircraft, assignments) {
  aircraft <- fse.getAircraft(rentalAircraft$MakeModel[1])
  assignments$GroundCrewFee <- sapply(1:nrow(assignments), function(n) {calc.groundCrewFee(assignments[n,])})
  assignments$FuelUsage <- sapply(1:nrow(assignments), function(n) {calc.fuelUsage(aircraft, assignments$Distance[n])})
  assignments$Duration <- sapply(1:nrow(assignments), function(n) {calc.duration(aircraft, assignments$Distance[n])})
  assignments$DistanceBonus <- sapply(1:nrow(assignments), function(n) {
    ac <- rentalAircraft[rentalAircraft$Location == assignments$Location[n],][1,]
    calc.distanceBonus(ac, assignments[n,])
  })
  assignments$DryEarnings <- sapply(1:nrow(assignments), function(n) {
    ac <- rentalAircraft[rentalAircraft$Location == assignments$Location[n],][1,]
    calc.earnings(ac, aircraft, assignments[n,])
  })
  assignments$WetEarnings <- sapply(1:nrow(assignments), function(n) {
    ac <- rentalAircraft[rentalAircraft$Location == assignments$Location[n],][1,]
    calc.earnings(ac, aircraft, assignments[n,], dry = FALSE)
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

  # TODO: Include booking fee. https://sites.google.com/site/fseoperationsguide/getting-started/assignments#TOC-Assignment-Fees
  
  return (assignment$Pay - cost + assignment$DistanceBonus - assignment$GroundCrewFee)
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

calc.distanceBonus <- function(rentalAircraft, assignment) {
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
