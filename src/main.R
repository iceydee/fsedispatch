cat("FSEDispatch v0.1.0\n")
cat("------------------\n\n")

source("./src/fseconomy.R")
source("./src/earnings.R")
source("./src/regions.R")

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

# Get the aircraft
aircraftList <- fse.getAircraft()
printVector(aircraftList$MakeModel)
q <- sprintf("\nWhich aircraft? [1-%i] ", nrow(aircraftList))
aircraft <- aircraftList[readInt(q),]

# Get the distance range
minDistance <- readInt("What's the minimum range you want to fly today? [1-?] ")
maxDistance <- readInt(sprintf("What's the maximum range you want to fly today? [%i-?] ", minDistance + 1))

# Find possible rental aircraft
rentalAircraft <- fse.findRentalAircraft(
  aircraft$MakeModel,
  waterOk = FALSE
)

# Check how many ac we have in each region
regions$count <- sapply(1:nrow(regions), function (n) {
  return (nrow(limitByRegion(rentalAircraft, regions[n,])))
})

# Get the region
acExists <- FALSE
for (n in 1:nrow(regions)) {
  if (regions$count[n] > 0) {
    acExists <- TRUE
    cat(sprintf("[%i] %s (%i aircraft rentable)\n", n, regions$name[n], regions$count[n]))
  }
}
if (!acExists) {
  stop(sprintf("No '%s' available for rent.", aircraft$MakeModel))
}
q <- sprintf("\nWhich region? [1-%i] ", nrow(regions))
region <- regions[readInt(q),]
if (region$count < 1) {
  stop(sprintf("No '%s' available for rent in %s.", aircraft$MakeModel, region$name))
}
rentalAircraft <- limitByRegion(rentalAircraft, region)

# Find assignments
leg1 <- getRankedAssignments(rentalAircraft, 0, maxDistance)
leg2 <- getRankedAssignments(rentalAircraft, minDistance, maxDistance, leg1$ToIcao, leg1$FromIcao)

printOption <- function(result) {
  amount2 <- 0
  distance2 <- 0
  fuelUsage2 <- 0
  duration2 <- 0
  commidity2 <- ""
  cat(sprintf("Get your %s from %s\n", aircraft$MakeModel, result$start))
  if (!is.na(result$mid)) {
    cat(sprintf("Fly %i %s to %s. (%i nm distance)\n", result$amount1, result$commodity1, result$mid, result$distance1))
    cat(sprintf("Bring minimum %.0f gal fuel (with zero wind, no reserve)\n", result$fuelUsage1))
    cat(sprintf("Estimated duration %.2fh (zero wind, no taxi time)\n", result$duration1))
    cat("\nThen\n\n")
    amount2 <- result$amount2
    distance2 <- result$distance2
    fuelUsage2 <- result$fuelUsage2
    duration2 <- result$duration2
    commodity2 <- result$commodity2
  } else {
    amount2 <- result$amount1
    distance2 <- result$distance1
    fuelUsage2 <- result$fuelUsage2
    duration2 <- result$duration1
    commodity2 <- result$commodity1
  }
  cat(sprintf("Fly %i %s to %s. (%i nm distance)\n", amount2, commodity2, result$end, distance2))
  cat(sprintf("Bring minimum %.0f gal fuel (with zero wind, no reserve)\n", fuelUsage2))
  cat(sprintf("Estimated duration %.2fh (zero wind, no taxi time)\n", duration2))
  cat(sprintf("\nTotal distance: %i nm\n", result$totalDistance))
  cat(sprintf("Total earnings: $%.0f\n", result$totalEarnings))
  cat(sprintf("Total duration: %.2fh\n", result$totalDuration))
}

results <- gatherResults(leg1, leg2)
cat(rep("\n", 20))
cat("Option 1:\n")
printOption(results[1,])
cat("\n\nOption 2:\n")
printOption(results[2,])
