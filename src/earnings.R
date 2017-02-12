source("./src/fseconomy.R")

calc.assignments <- function(makeModel, assignments) {
  aircraft <- fse.getAircraft(makeModel)
  assignments$FuelUsage <- sapply(1:nrow(assignments), function(n) {calc.fuelUsage(aircraft, assignments$Distance[n])})
  assignments$Duration <- sapply(1:nrow(assignments), function(n) {calc.duration(aircraft, assignments$Distance[n])})
  assignments$DryEarnings <- sapply(1:nrow(assignments), function(n) {calc.earnings(aircraft, assignments[n,])})
  assignments$WetEarnings <- sapply(1:nrow(assignments), function(n) {calc.earnings(aircraft, assignments[n,], dry = FALSE)})
  assignments$Earnings <- sapply(1:nrow(assignments), function(n) {max(assignments$DryEarnings[n], assignments$WetEarnings[n])})
  return (assignments)
}

calc.earnings <- function(aircraft, assignment, dry = TRUE) {
  if (assignment$RentalWet == 0) {
    dry <- TRUE
  }
  if (assignment$RentalDry == 0) {
    dry <- FALSE
  }
  
  cost <- 0
  if (dry) {
    fuelCost <- (calc.fuelUsage(aircraft, assignment$Distance) * assignment$FuelPrice)
    cost <- ((assignment$RentalDry * assignment$Duration) + fuelCost)
  } else {
    cost <- (assignment$RentalWet * assignment$Duration)
  }

  # TODO: Include ground crew fee + booking fee. https://sites.google.com/site/fseoperationsguide/getting-started/assignments#TOC-Assignment-Fees
  # TODO: Include distance bonus
  
  return (assignment$Pay - cost)
}

calc.fuelUsage <- function(aircraft, distance) {
  return (aircraft$GPH * calc.duration(aircraft, distance))
}

calc.duration <- function(aircraft, distance) {
  # TODO: Include taxi, climb, descent
  return (as.integer(distance) / as.integer(aircraft$CruiseSpeed))
}
