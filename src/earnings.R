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

calc.maxFuel <- function(aircraft, trafficWeight) {
  return (fuelVolume(calc.maxPayload(aircraft) - trafficWeight))
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

gatherResults <- function(legs, maxDistance, destination = NA, destinationWeight = 0.5) {
  r <- list()
  
  for (n in 1:nrow(legs[[1]])) {
    a <- list()
    a[[1]] <- legs[[1]][n,]
    if (length(legs) > 1) {
      curDistance <- a[[1]]$Distance
      for (i in 2:length(legs)) {
        b <- legs[[i]][legs[[i]]$FromIcao == legs[[i - 1]]$ToIcao,]
        maxBDistance <- maxDistance - curDistance
        b <- b[b$Distance < maxBDistance,]
        b <- b[order(-b$Earnings),]
        a[[i]] <- b[1,]
        curDistance <- curDistance + a[[i]]$Distance
      }
    }
    
    entry <- data.frame(
      start = character(),
      end = character(),
      seats = integer(),
      hasCargo = logical(),
      weight = integer(),
      takeOffWeight = integer(),
      earnings = integer(),
      costOfDelay = integer(),
      dry = logical(),
      assignmentIds = character(),
      distance = integer(),
      fuelUsage = integer(),
      maxFuel = integer(),
      duration = integer(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:length(a)) {
      b <- a[[i]]
      entry[i,] <- list(
        start = b$FromIcao,
        end = b$ToIcao,
        seats = b$Seats,
        hasCargo = b$HasCargo,
        weight = b$Weight,
        takeOffWeight = b$TakeOffWeight,
        earnings = b$Earnings,
        costOfDelay = b$CostOfDelay,
        dry = is.dry(b),
        assignmentIds = b$AssignmentIds,
        distance = b$Distance,
        fuelUsage = ceiling(b$FuelUsage),
        maxFuel = floor(b$MaxFuel),
        duration = b$Duration
      )
    }
    
    entry$totalEarnings <- rep(sum(entry$Earnings), nrow(entry))
    entry$totalCostOfDelay <- rep(sum(entry$CostOfDelay), nrow(entry))
    entry$totalDistance <- rep(sum(entry$Distance), nrow(entry))
    entry$totalDuration <- rep(sum(entry$Duration), nrow(entry))
    
    r[[n]] <- as.data.frame(entry)
  }
  
  earnings <- list()
  for (n in 1:length(results)) {
    earnings[[n]] <- r[[n]]$totalEarnings[1]
  }
  earnings <- rapply(earnings, c)
  order <- sort(earnings, decreasing = T, index.return = T)$ix
  
  results <- list()
  for (n in 1:length(order)) {
    results[[n]] <- r[[order[n]]]
  }
  return (results)
  
  # TODO: For when we have own aircraft
  # if (!is.na(destination)) {
  #   # Calculate destination bias
  #   topEarn <- results[1,]$totalEarnings
  #   destinationBonus <- topEarn * destinationWeight
  # 
  #   results$destinationValue <- sapply(1:nrow(results), function(n) {
  #     calc.destinationBonus(destination, destinationBonus, results$start[n], results$end[n]) + results$totalEarnings[n]
  #   })
  # 
  #   results <- results[order(-results$destinationValue),]
  # }
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

getRankedAirlineAssignments <- function(rentalAircraft, minDistance = 50, maxDistance = 400, progress = function(a, b) {}) {
  aircraft <- fse.getAircraft(rentalAircraft$MakeModel[1])
  searchICAO <- unique(sort(rentalAircraft$Location))
  assignments <- fse.getAirlineAssignments(searchICAO, progress)
  
  # Filter aircraft and distance
  assignments <- assignments[assignments$Aircraft == aircraft$MakeModel,]
  assignments <- assignments[assignments$Distance >= minDistance & assignments$Distance <= maxDistance,]
  
  if (nrow(assignments) < 1) {
    return (assignments)
  }
  
  # Calculate final earnings
  assignments <- calc.airlineAssignments(rentalAircraft, assignments)
  
  # And sort
  assignments <- assignments[order(-assignments$Earnings),]
  return (assignments)
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
  legs[[1]] <- getLeg(rentalAircraft$Location, aircraft, minDistance, maxDistance, progress)
  if (maxHops > 1) {
    for (n in 2:maxHops) {
      legs[[n]] <- getLeg(legs[[n - 1]]$ToIcao, aircraft, minDistance, maxDistance, progress)
    }
  }
  
  # Create assignment tree
  tree <- Node$new("start")
  addResultNodes(legs, tree)
  
  # Do calculations
  calc.treeAssignments(rentalAircraft, tree)
  
  # Prune
  Prune(tree, function(node) calc.totalDistance(node) < maxDistance)
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

getRankedAssignments <- function(rentalAircraft, minDistance = 50, maxDistance = 400, searchICAO = NULL, matchICAO = NULL, progress = function(a, b) {}) {
  aircraft <- fse.getAircraft(rentalAircraft$MakeModel[1])
  
  findNearestAC <- FALSE
  
  if (is.null(searchICAO) || is.na(searchICAO)) {
    searchICAO <- rentalAircraft$Location
  } else {
    findNearestAC <- TRUE
  }
  
  # Find assignments
  maxSeats <- (aircraft$Seats - 1 - aircraft$Crew) # TODO: Use fuel/weight calc in case we end up overweight
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

calc.airlineAssignments <- function(rentalAircraft, assignments) {
  aircraft <- fse.getAircraft(rentalAircraft$MakeModel[1])
  fse.fetchAirports(c(assignments$FromIcao, assignments$ToIcao))
  assignments$GroundCrewFee <- sapply(1:nrow(assignments), function(n) {calc.groundCrewFee(assignments[n,])})
  assignments$FuelUsage <- sapply(1:nrow(assignments), function(n) {calc.fuelUsage(aircraft, assignments$Distance[n])})
  assignments$HasPassengers <- rep(T, nrow(assignments))
  assignments$HasCargo <- rep(F, nrow(assignments))
  assignments$Duration <- sapply(1:nrow(assignments), function(n) {calc.duration(aircraft, assignments$Distance[n])})
  assignments$Earnings <- sapply(1:nrow(assignments), function(n) {assignments$Pay[n] - assignments$GroundCrewFee[n]})
  assignments$WetEarnings <- sapply(1:nrow(assignments), function(n) {assignments$Earnings[n]})
  assignments$DryEarnings <- sapply(1:nrow(assignments), function(n) {assignments$Earnings[n]})
  assignments$CostOfDelay <- rep(0.0, nrow(assignments))
  assignments$Amount <- sapply(1:nrow(assignments), function(n) {as.integer(gsub("[^0-9]+", "", assignments$Commodity[n]))})
  assignments$Seats <- assignments$Amount
  assignments$Weight <- sapply(1:nrow(assignments), function(n) {
    passengerWeight(assignments$Seats)
  })
  assignments$TakeOffWeight <- sapply(1:nrow(assignments), function(n) {
    calc.takeOffWeight(aircraft, fuelWeight(assignments$FuelUsage[n], aircraft$FuelType), assignments$Weight[n])
  })
  assignments$MaxFuel <- sapply(1:nrow(assignments), function(n) {
    calc.maxFuel(aircraft, assignments$Weight[n])
  })
  return (assignments)
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
  current <- current + node$Duration
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
    self$WetEarnings <- {
      x <- ac[ac$RentalWet > 0,]
      x <- x[order(x$RentalWet),]
      if (nrow(x) < 1) {
        NA
      } else {
        calc.earnings(x[1,], aircraft, self)
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

calc.assignments <- function(rentalAircraft, assignments) {
  aircraft <- fse.getAircraft(rentalAircraft$MakeModel[1])
  fse.fetchAirports(c(assignments$FromIcao, assignments$ToIcao))
  assignments$GroundCrewFee <- sapply(1:nrow(assignments), function(n) {calc.groundCrewFee(assignments[n,])})
  assignments$BookingFee <- sapply(1:nrow(assignments), function(n) {calc.bookingFee(assignments[n,])})
  assignments$FuelUsage <- sapply(1:nrow(assignments), function(n) {calc.fuelUsage(aircraft, assignments$Distance[n])})
  assignments$TakeOffWeight <- sapply(1:nrow(assignments), function(n) {
    calc.takeOffWeight(aircraft, fuelWeight(assignments$FuelUsage[n], aircraft$FuelType), assignments$Weight[n])
  })
  assignments$MaxFuel <- sapply(1:nrow(assignments), function(n) {
    calc.maxFuel(aircraft, assignments$Weight[n])
  })
  assignments$Duration <- sapply(1:nrow(assignments), function(n) {calc.duration(aircraft, assignments$Distance[n])})
  assignments$DistanceBonus <- sapply(1:nrow(assignments), function(n) {
    ac <- rentalAircraft[rentalAircraft$Location == assignments$Location[n],]
    if (nrow(ac) < 1) {
      cat(sprintf("NB! For real, no rental aircraft found for %s: %s\n", assignments$Location[n], paste(rentalAircraft$Location, collapse = "-")))
      return (0)
    }
    calc.distanceBonus(ac[1,], assignments[n,])
  })
  assignments$DryEarnings <- sapply(1:nrow(assignments), function(n) {
    ac <- rentalAircraft[rentalAircraft$Location == assignments$Location[n],]
    ac <- ac[ac$RentalDry > 0,]
    ac <- ac[order(ac$RentalDry),]
    if (nrow(ac) < 1) {
      return (NA)
    }
    calc.earnings(ac[1,], aircraft, assignments[n,])
  })
  assignments$WetEarnings <- sapply(1:nrow(assignments), function(n) {
    ac <- rentalAircraft[rentalAircraft$Location == assignments$Location[n],]
    ac <- ac[ac$RentalWet > 0,]
    ac <- ac[order(ac$RentalWet),]
    if (nrow(ac) < 1) {
      return (NA)
    }
    calc.earnings(ac[1,], aircraft, assignments[n,], dry = FALSE)
  })
  assignments$Earnings <- sapply(1:nrow(assignments), function(n) {
    e <- c(dry = assignments$DryEarnings[n], wet = assignments$WetEarnings[n])
    e[is.na(e)] <- -Inf
    max(e["dry"], e["wet"])
  })
  assignments$CostOfDelay <- sapply(1:nrow(assignments), function(n) {
    ac <- rentalAircraft[rentalAircraft$Location == assignments$Location[n],]
    e <- c(dry = assignments$DryEarnings[n], wet = assignments$WetEarnings[n])
    e[is.na(e)] <- -Inf
    if (e["dry"] > e["wet"]) {
      dry <- T
    } else {
      dry <- F
    }
    if (nrow(ac) < 1) {
      cat(sprintf("No rental aircraft found for %s: %s\n", assignments$Location[n], paste(rentalAircraft$Location, collapse = "-")))
      return (0.0)
    }
    delayedEarnings <- calc.earnings(ac[1,], aircraft, assignments[n,], dry = dry, delay = 1.0)
    return (assignments$Earnings[n] - delayedEarnings)
  })
  return (assignments)
}

calc.earnings <- function(rentalAircraft, aircraft, assignment, dry = TRUE, delay = 0.0) {
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
