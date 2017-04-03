library("data.tree")
source("./src/fseconomy.R")

fuelWeight <- function(vol, type = 0) {
  if (type == 0) { # 100LL
    return (vol * 2.72)
  }
  return (vol * 3.08)
}

fuelVolume <- function(weight, type = 0) {
  if (type == 0) { # 100LL
    return (weight / 2.72)
  }
  return (weight / 3.08)
}

calc.takeOffWeight <- function(aircraft, fuelWeight, trafficWeight) {
  return (aircraft$EmptyWeight + fuelWeight + trafficWeight)
}

aircraftMaxFuel <- function(aircraft) {
  return (aircraft$Ext1 +
          aircraft$LTip +
          aircraft$LAux +
          aircraft$LMain +
          aircraft$Center1 +
          aircraft$Center2 +
          aircraft$Center3 +
          aircraft$RMain +
          aircraft$RAux +
          aircraft$RTip +
          aircraft$Ext2)
}

calc.maxFuel <- function(aircraft, trafficWeight) {
  weightMax <- fuelVolume(calc.maxPayload(aircraft) - trafficWeight)
  acMax <- aircraftMaxFuel(aircraft)
  return (min(weightMax, acMax))
}

calc.maxPayload <- function(aircraft) {
  return (aircraft$MTOW - aircraft$EmptyWeight)
}

calc.maxCargo <- function(aircraft, volFuel, passengers = 0) {
  return (calc.maxPayload(aircraft) - fuelWeight(volFuel, aircraft$FuelType) - passengerWeight(passengers))
}

calc.maxPassengers <- function(aircraft, volFuel, cargo = 0) {
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

getLeg <- function(searchICAO, aircraft, minDistance = 50, maxDistance = 400, progress = function(a, b) {}) {
  maxSeats <- (aircraft$Seats - 1 - aircraft$Crew)
  maxCargo <- calc.maxCargo(aircraft, calc.fuelUsage(aircraft, maxDistance))
  
  assignments <- fse.getAssignments(
    searchICAO,
    minDistance = minDistance, maxDistance = maxDistance,
    maxSeats = maxSeats, maxCargo = maxCargo,
    grouped = TRUE,
    progress = progress
  )
  
  if (length(assignments) < 1) {
    return (data.frame())
  }
  
  groupedAssignments <- assignments[[1]][0,]
  groupedAssignments["PtCount"] <- integer(0)
  groupedAssignments["AssignmentIds"] <- character(0)
  groupedAssignments["HasPassengers"] <- logical(0)
  groupedAssignments["HasCargo"] <- logical(0)
  
  for (n in 1:length(assignments)) {
    a <- assignments[[n]]
    b <- a[1,]
    b$Id <- n
    b$Amount <- sum(a$Amount)
    b$Seats <- sum(a$Seats)
    b$Weight <- sum(a$Weight)
    b$Pay <- sum(a$Pay)
    b$PtCount <- nrow(a[a$PtAssignment == "true",])
    b$AssignmentIds <- paste(a$Id, collapse = ",")
    b$HasPassengers <- isTRUE(b$Seats > 0)
    b$HasCargo <- isTRUE(nrow(a[a$UnitType == "kg",]) > 0)
    b$PilotForHire <- F
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

  return (groupedAssignments)  
}

addResultNodes <- function(legs, rootNode, node = NA, fromIcao = NA, level = 1, depth = Inf) {
  a <- legs[[level]]
  if (!is.na(fromIcao)) {
    a <- a[a$FromIcao == fromIcao,]
  }
  if (nrow(a) < 1) {
    return ()
  }
  for (n in 1:min(nrow(a), depth)) {
    if (level == 1) {
      a$pathString[n] <- sprintf("S1-%s-%s", a$FromIcao[n], a$ToIcao[n])
      node <- as.Node(a[n,])
      rootNode$AddChildNode(node)
    }
    a$pathString[n] <- sprintf("L%.0f-%s", level, a$ToIcao[n])
    b <- as.Node(a[n,])
    node$AddChildNode(b)
    if (level < length(legs)) {
      addResultNodes(legs, rootNode, b, a$ToIcao[n], level + 1, depth)
    }
  }
}

pruneNonEarningLeaves <- function(tree) {
  for (node in Traverse(tree, filterFun = isLeaf)) {
    if (node$Earnings < 0) {
      node$keep <- F
    } else {
      node$keep <- T
    }
  }
  Prune(tree, function(node) {return (is.null(node$keep) || node$keep)})
  Prune(tree, function(node) {return (!(node$parent$isRoot && node$isLeaf))})
}

getAssignmentTree <- function(rentalAircraft, minDistance = 50, maxDistance = 400, maxHops = 3, progress = function(a, b) {}) {
  aircraft <- fse.getAircraft(rentalAircraft$MakeModel[1])
  
  # Fetch assignment legs
  legs <- list()
  legLength <- length(rentalAircraft$Location)
  legs[[1]] <- getLeg(rentalAircraft$Location, aircraft, 0, maxDistance, function(v, m) {
    progress(v, legLength)
  })
  if (maxHops > 1) {
    for (n in 2:maxHops) {
      legLength <- length(legs[[n - 1]]$ToIcao)
      legs[[n]] <- getLeg(legs[[n - 1]]$ToIcao, aircraft, 0, maxDistance, function(v, m) {
        progress(v + (n-1), legLength)
      })
    }
  }
  
  progress(maxHops, 1)
  
  # Create assignment tree
  tree <- Node$new("start")
  addResultNodes(legs, tree)
  
  # Do calculations
  calc.treeAssignments(rentalAircraft, tree)
  
  # Prune
  Prune(tree, function(node) calc.totalDistance(node) <= maxDistance)
  Prune(tree, function(node) {
    if (!node$isLeaf) {
      return (T)
    }
    calc.totalDistance(node) >= minDistance
  })
  for (n in 1:maxHops) {
    # Prune leaves maxdepth
    pruneNonEarningLeaves(tree)
  }
  
  # Add totals to leaves
  for (node in Traverse(tree, filterFun = isLeaf)) {
    node$TotalEarnings <- calc.totalEarnings(node)
    node$TotalDistance <- calc.totalDistance(node)
    node$TotalCostOfDelay <- calc.totalCostOfDelay(node)
    node$TotalBlockTime <- calc.totalBlockTime(node)
  }
  
  return (tree)
}

getAirlineAssignmentTree <- function(rentalAircraft, minDistance = 50, maxDistance = 400, progress = function(a, b) {}) {
  aircraft <- fse.getAircraft(rentalAircraft$MakeModel[1])
  searchICAO <- unique(sort(rentalAircraft$Location))
  assignments <- fse.getAirlineAssignments(searchICAO, progress)
  
  progress(1, 1)
  
  # Filter aircraft and distance
  assignments <- assignments[assignments$Aircraft == aircraft$MakeModel,]
  assignments <- assignments[assignments$Distance >= minDistance & assignments$Distance <= maxDistance,]
  
  if (nrow(assignments) < 1) {
    return (Node$new("start"))
  }
  
  # Add attributes
  assignments$PtCount <- rep(0, nrow(assignments))
  assignments$AssignmentIds <- assignments$Id
  assignments$Amount <- sapply(1:nrow(assignments), function(n) {as.integer(gsub("[^0-9]+", "", assignments$Commodity[n]))})
  assignments$Seats <- assignments$Amount
  assignments$Weight <- sapply(1:nrow(assignments), function(n) {
    passengerWeight(assignments$Seats[n])
  })
  assignments$HasPassengers <- rep(T, nrow(assignments))
  assignments$HasCargo <- rep(F, nrow(assignments))
  assignments$PilotForHire <- rep(T, nrow(assignments))
  
  # Create assignment tree
  legs <- list()
  legs[[1]] <- assignments
  tree <- Node$new("start")
  addResultNodes(legs, tree)
  
  # Do calculations
  calc.treeAssignments(rentalAircraft, tree)
  
  # Add totals to leaves
  for (node in Traverse(tree, filterFun = isLeaf)) {
    node$TotalEarnings <- calc.totalEarnings(node)
    node$TotalDistance <- calc.totalDistance(node)
    node$TotalCostOfDelay <- calc.totalCostOfDelay(node)
    node$TotalBlockTime <- calc.totalBlockTime(node)
  }
  
  return (tree)
}

getRentalAircraftForNode <- function(rentalAircraft, node) {
  if (node$parent$isRoot) {
    return (rentalAircraft[rentalAircraft$Location == node$FromIcao,])
  }
  return (getRentalAircraftForNode(rentalAircraft, node$parent))
}

calc.totalEarnings <- function(node, current = 0.0) {
  if (node$parent$isRoot) {
    return (current)
  }
  current <- current + node$Earnings
  return (calc.totalEarnings(node$parent, current))
}

calc.totalDistance <- function(node, current = 0.0) {
  if (node$parent$isRoot) {
    return (current)
  }
  current <- current + node$Distance
  return (calc.totalDistance(node$parent, current))
}

calc.totalCostOfDelay <- function(node, current = 0.0) {
  if (node$parent$isRoot) {
    return (current)
  }
  current <- current + node$CostOfDelay
  return (calc.totalCostOfDelay(node$parent, current))
}

calc.totalBlockTime <- function(node, current = 0.0) {
  if (node$parent$isRoot) {
    return (current)
  }
  current <- current + ceiling((node$Duration * 60))
  return (calc.totalBlockTime(node$parent, current))
}

calc.treeAssignments <- function(rentalAircraft, tree) {
  aircraft <- fse.getAircraft(rentalAircraft$MakeModel[1])
  
  x <- c(as.vector(tree$Get("FromIcao")), as.vector(tree$Get("ToIcao")))
  fse.fetchAirports(x[!is.na(x)])
  
  tree$Do(function(self) {
    if (self$isRoot) {
      return ()
    }
    
    ac <- getRentalAircraftForNode(rentalAircraft, self)
    
    self$GroundCrewFee <- calc.groundCrewFee(self)
    self$BookingFee <- calc.bookingFee(self)
    self$FuelUsage <- calc.fuelUsage(aircraft, self$Distance)
    self$TakeOffWeight <- calc.takeOffWeight(aircraft, fuelWeight(self$FuelUsage, aircraft$FuelType), self$Weight)
    self$MaxFuel <- calc.maxFuel(aircraft, self$Weight)
    self$Duration <- calc.duration(aircraft, self$Distance)
    self$DistanceBonus <- calc.distanceBonus(ac[1,], self)
    self$DryEarnings <- {
      x <- ac[ac$RentalDry > 0,]
      x <- x[order(x$RentalDry),]
      if (nrow(x) < 1) {
        NA
      } else {
        calc.earnings(x[1,], aircraft, self)
      }
    }
    if (self$PilotForHire) {
      self$WetEarnings <- {
        calc.earnings(ac[1,], aircraft, self)
      }
    } else {
      self$WetEarnings <- {
        x <- ac[ac$RentalWet > 0,]
        x <- x[order(x$RentalWet),]
        if (nrow(x) < 1) {
          NA
        } else {
          calc.earnings(x[1,], aircraft, self)
        }
      }
    }
    
    e <- c(dry = self$DryEarnings, wet = self$WetEarnings)
    e[is.na(e)] <- -Inf
    
    self$Earnings <- max(e["dry"], e["wet"])
    self$RentDry <- is.dry(self)
    self$CostOfDelay <- {
      if (!is.na(self$RentDry) && self$RentDry) {
        x <- ac[ac$RentalDry > 0,]
        x <- x[order(x$RentalDry),]
      } else if (!is.na(self$RentDry)) {
        x <- ac[ac$RentalWet > 0,]
        x <- x[order(x$RentalWet),]
      } else {
        x <- ac[0,]
      }
      if (nrow(x) < 1) {
        NA
      } else {
        delayedEarnings <- calc.earnings(x[1,], aircraft, self, dry = self$RentDry, delay = 1.0)
        self$Earnings - delayedEarnings
      }
    }
  })
}

calc.earnings <- function(rentalAircraft, aircraft, assignment, dry = TRUE, delay = 0.0) {
  if (assignment$PilotForHire) {
    return (assignment$Pay - assignment$GroundCrewFee)
  }
  
  if (rentalAircraft$RentalWet == 0) {
    dry <- TRUE
  }
  if (rentalAircraft$RentalDry == 0) {
    dry <- FALSE
  }
  
  distance <- assignment$Distance
  duration <- assignment$Duration
  
  if (delay > 0) {
    distance <- (distance + (delay * aircraft$CruiseSpeed))
    duration <- (duration + delay)
  }
  
  cost <- 0
  if (dry) {
    fuelPrice <- assignment$FuelPrice
    if (is.null(fuelPrice) || is.na(fuelPrice)) {
      fuelPrice <- 4.5 # Default to sane default if there's no fuel at ICAO
    }
    fuelCost <- (calc.fuelUsage(aircraft, distance) * fuelPrice)
    cost <- ((rentalAircraft$RentalDry * duration) + fuelCost)
  } else {
    cost <- (rentalAircraft$RentalWet * duration)
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
  if (assignment$PilotForHire) {
    return (0.0)
  }
  return (calc.destinationBonus(rentalAircraft$Home, rentalAircraft$Bonus, assignment$FromIcao, assignment$ToIcao))
}

calc.destinationBonus <- function(destination, bonus, fromIcao, toIcao) {
  if (is.na(destination) ||
      is.na(fromIcao) ||
      is.na(toIcao)) {
    return (0.00)
  }
  
  oldDistance <- icao.distance(fromIcao, destination)
  newDistance <- icao.distance(toIcao, destination)
  distanceDiff <- oldDistance - newDistance
  return (bonus * distanceDiff / 100)
}

calc.fuelUsage <- function(aircraft, distance) {
  return (aircraft$GPH * calc.duration(aircraft, distance))
}

calc.duration <- function(aircraft, distance, taxi = 20) {
  # TODO: Include climb, descent
  return ((as.integer(distance) / as.integer(aircraft$CruiseSpeed)) + (taxi / 60))
}
