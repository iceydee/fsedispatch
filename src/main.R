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
assignments <- getRankedAssignments(rentalAircraft, 0, maxDistance)
hop2 <- getRankedAssignments(rentalAircraft, minDistance, maxDistance, searchICAO = assignments$ToIcao, matchICAO = assignments$FromIcao)

results <- data.frame(
  start = character(),
  mid = character(),
  end = character(),
  amount1 = integer(),
  commodity1 = character(),
  amount2 = integer(),
  commotity2 = character(),
  totalEarnings = integer(),
  distance1 = integer(),
  distance2 = integer(),
  totalDistance = integer(),
  stringsAsFactors = FALSE)
for (n in 1:nrow(assignments)) {
  a <- assignments[n,]
  b <- hop2[hop2$FromIcao == a$ToIcao,]
  maxBDistance <- maxDistance - a$Distance
  b <- b[b$Distance < maxBDistance,]
  b <- b[order(-b$Earnings),]
  
  if (nrow(b) > 0) {
    results[n,] <- list(
      start = a$FromIcao,
      mid = a$ToIcao,
      end = b$ToIcao,
      amount1 = a$Amount,
      commodity1 = a$Commodity,
      amount2 = b$Amount,
      commodity2 = b$Commodity,
      totalEarnings = (a$Earnings + b$Earnings),
      distance1 = a$Distance,
      distance2 = b$Distance,
      totalDistance = a$Distance + b$Distance
    )
  } else {
    results[n,] <- list(
      start = a$FromIcao,
      end = a$ToIcao,
      amount = a$Amount,
      commodity = a$Commodity,
      totalEarnings = a$Earnings,
      distance1 = a$Distance,
      totalDistance = a$Distance
    )
  }
}

# hop2 <- list()
# for (n in 1:nrow(assignments)) {
#   a <- assignments[n,]
#   if (a$Distance < maxDistance) {
#     ac <- rentalAircraft[rentalAircraft$Location == a$FromIcao,][1,]
#     minD <- max(minDistance - a$Distance, 0)
#     maxD <- (maxDistance - a$Distance)
#     b <- getRankedAssignments(ac, minD, maxD, searchICAO = a$ToIcao)
#     if (nrow(b) < 1) {
#       hop2[[(length(hop2) + 1)]] <- a[0,]
#       next
#     }
#     hop2[[(length(hop2) + 1)]] <- b[1:min(nrow(b), 5)]
#   } else {
#     hop2[[(length(hop2) + 1)]] <- a[0,]
#   }
# }

# results <- data.frame(
#   start = character(),
#   mid = character(),
#   end = character(),
#   amount1 = integer(),
#   commodity1 = character(),
#   amount2 = integer(),
#   commotity2 = character(),
#   totalEarnings = integer(),
#   distance1 = integer(),
#   distance2 = integer(),
#   totalDistance = integer(),
#   stringsAsFactors = FALSE)
# for (n in 1:nrow(assignments)) {
#   a <- assignments[n,]
#   b <- hop2[[n]][1,]
#   if (nrow(b) > 0) {
#     results[n,] <- list(
#       start = a$FromIcao[n],
#       mid = a$ToIcao[n],
#       end = b$ToIcao,
#       amount1 = a$Amount[n],
#       commodity1 = a$Commodity[n],
#       amount2 = b$Amount,
#       commodity2 = b$Commodity,
#       totalEarnings = a$Earnings[n] + b$Earnings,
#       distance1 = a$Distance[n],
#       distance2 = b$Distance,
#       totalDistance = a$Distance[n] + b$Distance
#     )
#   } else {
#     results[n,] <- list(
#       start = a$FromIcao[n],
#       end = a$ToIcao[n],
#       amount = a$Amount[n],
#       commodity = a$Commodity[n],
#       totalEarnings = a$Earnings[n],
#       distance1 = a$Distance[n],
#       totalDistance = a$Distance[n]
#     )
#   }
# }

results <- results[order(-results$totalEarnings),]
print(head(results))