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
assignments <- getRankedAssignments(rentalAircraft, minDistance, maxDistance)

print(head(assignments))
