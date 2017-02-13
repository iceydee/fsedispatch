cat("FSEDispatch v0.1.0\n")
cat("------------------\n\n")

source("./src/fseconomy.R")
source("./src/earnings.R")

readInt <- function(prompt = "") {
  cat(prompt)
  i <- file("stdin")
  a <- as.integer(readLines(i, 1))
  close(i)
  return (a)
}

# Get the aircraft
aircraftList <- fse.getAircraft()
print(aircraftList$MakeModel)
q <- sprintf("\nWhich aircraft? [1-%i] ", nrow(aircraftList))
aircraft <- aircraftList[readInt(q),]

# Get the distance range
minDistance <- readInt("What's the minimum range you want to fly today? [1-?] ")
maxDistance <- readInt(sprintf("What's the maximum range you want to fly today? [%i-?] ", minDistance + 1))

# Get the longitude range
minLon <- readInt("What's the most western longitude you want to fly from today? [-180 - 180] ")
maxLon <- readInt(sprintf("What's the most eastern longitude you want to fly from today? [%i - 180] " , minLon + 1))

cat("\n")

# Find possible rental aircraft
rentalAircraft <- fse.findRentalAircraft(aircraft$MakeModel, lonFilter = c(minLon, maxLon), waterOk = FALSE)

# Find assignments
maxSeats <- (aircraft$Seats - 1)
assignments <- fse.getAssignments(
  rentalAircraft$Location,
  minDistance = minDistance, maxDistance = maxDistance,
  maxSeats = maxSeats,
  grouped = TRUE
)

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

print(head(groupedAssignments))
