library(shiny)
library(datasets)
library(leaflet)
source("./src/regions.R")
source("./src/fseconomy.R")
source("./src/earnings.R")
source("./src/icao.R")
source("./src/scrape.R")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  values <- reactiveValues(
    optionNumber = 1,
    durationMin = Inf,
    durationMax = Inf
  )
  
  output$duration <- renderText({
    if (is.infinite(values$durationMin)) {
      sprintf("Select aircraft to get block time estimation")
    } else {
      sprintf("Estimated block time %.0f min - %.0f min", values$durationMin * 60, values$durationMax * 60)
    }
  })
  
  icaoOutput <- function(icao) {
    x <- icao.get(icao)
    return (sprintf("%s (%s, %s)", x$icao, x$name, x$country))
  }
  
  output$regionSelect <- renderUI({
    if (nchar(input$aircraft) >= 1) {
      if (input$airline) {
        rentalAircraft <- fse.findAircraft(
          input$aircraft,
          waterOk = FALSE
        )
      } else {
        rentalAircraft <- fse.findRentalAircraft(
          input$aircraft,
          waterOk = FALSE
        )
      }
      
      # Check how many ac we have in each region
      regions$count <- sapply(1:nrow(regions), function (n) {
        return (nrow(limitByRegion(rentalAircraft, regions[n,])))
      })
      
      regions <- regions[regions$count > 0,]
    } else {
      regions$count <- rep(0, nrow(regions))
    }
    
    r <- sapply(1:nrow(regions), function(n) {
      sprintf("%s (%.0f aircraft)", regions$name[n], regions$count[n])
    })
    selectizeInput("region", NULL, c("", r),
                   options = list(placeholder = "Select a region"))
  })
  
  output$groupSelect <- renderUI({
    groups <- fse.getGroups()
    
    selectizeInput("group", "Assign to", c(groups$name))
  })
  
  destinationData <- reactive({
    c("", as.character(icaodata$icao))
  })
  
  output$destinationSelect <- renderUI({
    if (input$thirdLeg != "Destination") {
      return (NULL)
    }
    
    return (
      selectizeInput("destination", NULL, destinationData(),
                     options = list(placeholder = "Select target airport"))
    )
  })
  
  optionData <- function(result) {
    data <- data.frame(From = character(),
                       To = character(),
                       Distance = integer(),
                       Seats = integer(),
                       HasCargo = logical(),
                       `Traffic Load` = integer(),
                       Earnings = double(),
                       DelayCost = double(),
                       RentDry = logical(),
                       Duration = double(),
                       `Minimum Fuel` = integer(),
                       `Maximum Fuel` = integer(),
                       `Takeoff Weight` = integer(),
                       stringsAsFactors = F)
    
    for (n in 1:length(result)) {
      data[n,] <- list(
        From = icaoOutput(result[[n]]$FromIcao),
        To = icaoOutput(result[[n]]$ToIcao),
        Distance = result[[n]]$Distance,
        Seats = result[[n]]$Seats,
        HasCargo = result[[n]]$HasCargo,
        `Traffic Load` = result[[n]]$Weight,
        Earnings = result[[n]]$Earnings,
        DelayCost = result[[n]]$CostOfDelay,
        RentDry = result[[n]]$RentDry,
        Duration = result[[n]]$Duration,
        `Minimum Fuel` = result[[n]]$FuelUsage,
        `Maximum Fuel` = result[[n]]$MaxFuel,
        `Takeoff Weight` = result[[n]]$TakeOffWeight
      )
    }
    
    return (data)
  }
  
  addRouteLines <- function(map) {
    data <- mapData()
    
    m <- addPolylines(map, lng = c(data[1,c("Longitude")], data[2,c("Longitude")]),
                           lat = c(data[1,c("Latitude")], data[2,c("Latitude")]),
                           weight = 1.5, color = "black")
    
    if (nrow(data) > 2) {
      # Add route lines for all legs
      m <- addPolylines(m, lng = c(data[2,c("Longitude")], data[3,c("Longitude")]),
                           lat = c(data[2,c("Latitude")], data[3,c("Latitude")]),
                           weight = 1.5, color = "black")
    }
    
    return (m)
  }
  
  output$routeMap <- renderLeaflet({
    leaflet(data = mapData()) %>% addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, label = ~Location, color = ~Color, stroke = F, radius = 5, weight = 3, fillOpacity = 0.5) %>%
      addRouteLines()
  })
  
  outputOption <- function(result) {
    return (div(
      renderDataTable(optionData(result), options = list(paging = F, searching = F, info = F)),
      h5(sprintf("Total earnings: $%.0f", result[[length(result)]]$TotalEarnings)),
      h5(sprintf("Total cost of delay: $%.0f", result[[length(result)]]$TotalCostOfDelay)),
      h5(sprintf("Total distance: %.0f nm", result[[length(result)]]$TotalDistance)),
      h5(sprintf("Total block time: %.0f minutes", result[[length(result)]]$TotalDuration)),
      leafletOutput("routeMap")
    ))
  }
  
  is.empty <- function(v) {
    if (is.null(v) || nchar(v) < 1) {
      return (T)
    }
    return (F)
  }
  
  recalcDuration <- function() {
    ac <- fse.getAircraft(input$aircraft)
    if (nrow(ac) < 1) {
      values$durationMin <- Inf
      values$durationMax <- Inf
    } else {
      values$durationMin <- calc.duration(ac, input$distance[1])
      values$durationMax <- calc.duration(ac, input$distance[2])
    }
  }
  
  group <- reactive({
    # Dependencies
    input$group
    
    isolate(
      withProgress(message = "Fetching groups", value = 0, {
        groups <- fse.getGroups()
        group <- groups[groups$name == input$group,]
      })
    )
    
    return (group)
  })
  
  region <- reactive({
    region <- sub(" \\([0-9]+ aircraft\\)", "", input$region)
    return (regions[regions$name == region,])
  })
  
  rentalAircraft <- reactive({
    # Dependencies
    input$aircraft
    input$airline
    input$region
    input$distance
    
    isolate({
      if (input$airline) {
        rentalAircraft <- fse.findAircraft(input$aircraft, waterOk = F)
      } else {
        rentalAircraft <- fse.findRentalAircraft(input$aircraft, waterOk = F)
      }
      rentalAircraft <- limitByRegion(rentalAircraft, region())
    })
    
    return (rentalAircraft)
  })
  
  airlineResults <- reactive({
    # Dependencies
    input$aircraft
    input$region
    
    if (Sys.getenv("CANNED_DATA") == "true") {
      results <- readRDS("./data/canned_airline_data.rds")
      return (results)
    }
    
    isolate(
      withProgress(message = "Finding assignments", value = 0, {
        # Find rental aircraft
        setProgress(0.1, message = "Finding rental aircraft", detail = "")
        rentalAircraft <- rentalAircraft()
        
        # Fetch leg 1
        minDistance <- input$distance[1]
        maxDistance <- input$distance[2]
        leg1 <- getRankedAirlineAssignments(rentalAircraft, minDistance, maxDistance, progress = function(v, m) {
          setProgress(0.1 + (v * 0.9),
                      message = "Fetching airline assignments",
                      detail = sprintf("%.0f / %.0f", v * nrow(rentalAircraft), nrow(rentalAircraft)))
        })

        # Gather results
        setProgress(0.9, detail = "Gathering results")
        results <- gatherResults(leg1, leg1[0,], maxDistance)
      })
    )
    
    return (results)
  })
  
  rentalResults <- reactive({
    # Dependencies
    input$aircraft
    input$airline
    input$region
    input$distance
    input$hops
    
    if (Sys.getenv("CANNED_DATA") == "true") {
      results <- readRDS("./data/canned_data.rds")
      return (results)
    }
    
    isolate(
      withProgress(message = "Finding assignments", value = 0, {
        # Find rental aircraft
        setProgress(0.1, message = "Finding rental aircraft", detail = "")
        rentalAircraft <- rentalAircraft()

        # Fetch leg 1
        minDistance <- input$distance[1]
        maxDistance <- input$distance[2]
        
        pFact <- 1/input$hops
        tree <- getAssignmentTree(rentalAircraft, minDistance, maxDistance, input$hops, progress = function(v, m) {
          if (v == input$hops && m == 1) {
            setProgress(0.9,
                        message = "Finding most profitable routes",
                        detail = "")
          } else {
            legNo <- ceiling(v)
            v <- (v - (legNo - 1))
            legV <- v
            v <- ((v * pFact) + ((legNo - 1) * pFact))
            setProgress(0.1 + (v * 0.9),
                        message = sprintf("Fetching assignments (Leg %.0f)", legNo),
                        detail = sprintf("%.0f / %.0f", v * m, m))
          }
        })
      })
    )
    
    leaves <- Traverse(tree, filterFun = isLeaf)
    
    # Sort by total earnings
    l <- unlist(lapply(leaves, function(node) node$TotalEarnings))
    ix <- sort.int(l, decreasing = T, index.return = T)$ix
    result <- list()
    for (n in 1:length(ix)) {
      result[[n]] <- leaves[[ix[n]]]
    }
    
    return (result)
  })
  
  results <- reactive({
    # Dependencies
    input$aircraft
    input$airline
    input$region
    input$distance
    input$hops
    
    if (input$airline) {
      results <- airlineResults()
    } else {
      results <- rentalResults()
    }
    
    # Reset the option number
    values$optionNumber <- 1
    
    return (results)
  })
  
  currentOption <- reactive({
    if (numberOfResults() < 1) {
      return (list())
    }
    curNode <- results()[[values$optionNumber]]
    nodes <- list()
    while (!curNode$parent$isRoot) {
      nodes[[length(nodes)+1]] <- curNode
      curNode <- curNode$parent
    }
    return (rev(nodes))
  })
  
  numberOfResults <- reactive({
    results <- results()
    return (length(results))
  })
  
  mapData <- reactive({
    option <- currentOption()
    
    df <- data.frame(Location = character(), Color = character(), OrderNo = integer(), stringsAsFactors = F)
    for (n in 1:length(option)) {
      if (n == 1) {
        color <- "green"
      } else {
        color <- "blue"
      }
      
      df[n,] <- list(Location = option[[n]]$FromIcao, Color = color, OrderNo = n)
    }
    df[length(option) + 1,] <- list(Location = option[[length(option)]]$ToIcao, Color = "red", OrderNo = length(option) + 1)

    df <- merge(df, icaoloc, by = "Location")
    df <- df[order(df$OrderNo),]
    
    return (df)
  })
  
  output$results <- renderUI({
    # Dependencies
    if (is.empty(input$aircraft) || is.empty(input$region)) {
      return()
    }
    
    results <- results()
    if (length(results) < 1) {
      return (
        div(
          h2("No routes found with those settings")
        )
      )
    }
    
    div(
      h2("Results are in"),
      div(
        id = "currentOption",
        h3(sprintf("Option %.0f", values$optionNumber)),
        outputOption(currentOption()),
        actionButton("optionBook", "Book Option"),
        actionButton("acRent", "Rent Aircraft"),
        actionButton("prevOption", "Prev Option"),
        actionButton("nextOption", "Next Option")
      )
    )
  })
  
  observeEvent(input$aircraft, {
    recalcDuration()
  })
  
  observeEvent(input$distance, {
    recalcDuration()
  })
  
  observeEvent(input$optionBook, {
    withProgress(message = "Booking assignments", value = 0, {
      a <- currentOption()
      
      group <- group()
      
      for (n in 1:length(a)) {
        setProgress(0.1 + ((0.9 / nrow(a)) * n), detail = sprintf("Leg %.0f", n))
        fse.bookAssignments(a[[n]]$FromIcao, a[[n]]$AssignmentIds, group_id = group$groupId)
      }
    })
  })
  
  observeEvent(input$acRent, {
    withProgress(message = "Renting aircraft", value = 0, {
      a <- currentOption()
      
      rentalAircraft <- rentalAircraft()
      ac <- rentalAircraft[rentalAircraft$Location == a[[1]]$FromIcao,]
      if (a[[1]]$RentDry) {
        ac <- ac[order(ac$RentalDry),]
      } else {
        ac <- ac[order(ac$RentalWet),]
      }
      ac <- ac[1,]
      
      setProgress(0.1, detail = "Contacting FSE")
      
      fse.rentAircraft(ac$Location, ac$Registration, a[[1]]$RentDry)
    })
  })
  
  observeEvent(input$prevOption, {
    if (values$optionNumber > 1) {
      values$optionNumber <- values$optionNumber - 1
    }
  })
  
  observeEvent(input$nextOption, {
    if (values$optionNumber < numberOfResults()) {
      values$optionNumber <- values$optionNumber + 1
    }
  })
})
