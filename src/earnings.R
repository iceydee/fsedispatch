source("./src/fseconomy.R")

calc.assignments <- function(makeModel, assignments, rentalDry = 0, rentalWet = 0, fuelPrice = 0) {
  aircraft <- fse.getAircraft(makeModel)
  assignments$Duration <- sapply(1:nrow(assignments), function(n) {calc.duration(aircraft, assignments$Distance[n])})
  assignments$Earnings <- sapply(1:nrow(assignments), function(n) {calc.earnings(aircraft, assignments[n,], rentalDry = rentalDry, rentalWet = rentalWet, fuelPrice = fuelPrice)})
  return (assignments)
}

calc.earnings <- function(aircraft, assignment, rentalDry = 0, rentalWet = 0, fuelPrice = 0) {
  cost <- 0
  if (rentalDry > 0) {
    cost <- (cost + (calc.fuelUsage(aircraft, assignment$Distance) * fuelPrice))
  }
  cost <- (cost + rentalDry + rentalWet)
  
  # TODO: Include ground crew fee + booking fee. https://sites.google.com/site/fseoperationsguide/getting-started/assignments#TOC-Assignment-Fees
  
  return (assignment$Pay - cost)
}

calc.fuelUsage <- function(aircraft, distance) {
  return (aircraft$GPH * calc.duration(aircraft, distance))
}

calc.duration <- function(aircraft, distance) {
  return (distance / aircraft$CruiseSpeed)
}
