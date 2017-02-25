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
      rentalAircraft <- fse.findRentalAircraft(
        input$aircraft,
        waterOk = FALSE
      )
      
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
  
  oneLegOptionData <- function(result) {
    data <- list(From = character(),
                       To = character(),
                       Distance = integer(),
                       Amount = integer(),
                       Commodity = character(),
                       Earnings = double(),
                       RentDry = logical(),
                       Duration = double(),
                       `Minimum Fuel` = integer(),
                       stringsAsFactors = F)
    data[1,] <- data.frame(
      From = icaoOutput(result$start),
      To = icaoOutput(result$end),
      Distance = result$distance1,
      Amount = result$amount1,
      Commodity = result$commodity1,
      Earnings = result$earnings1,
      RentDry = result$dry1,
      Duration = result$duration1,
      `Minimum Fuel` = result$fuelUsage1
    )
    
    return (data)
  }
  
  twoLegOptionData <- function(result) {
    data <- data.frame(From = character(),
                       To = character(),
                       Distance = integer(),
                       Amount = integer(),
                       Commodity = character(),
                       Earnings = double(),
                       RentDry = logical(),
                       Duration = double(),
                       `Minimum Fuel` = integer(),
                       stringsAsFactors = F)
    data[1,] <- list(
      From = icaoOutput(result$start),
      To = icaoOutput(result$mid),
      Distance = result$distance1,
      Amount = result$amount1,
      Commodity = result$commodity1,
      Earnings = result$earnings1,
      RentDry = result$dry1,
      Duration = result$duration1,
      `Minimum Fuel` = result$fuelUsage1
    )
    data[2,] <- list(
      From = icaoOutput(result$mid),
      To = icaoOutput(result$end),
      Distance = result$distance2,
      Amount = result$amount2,
      Commodity = result$commodity2,
      Earnings = result$earnings2,
      RentDry = result$dry2,
      Duration = result$duration2,
      `Minimum Fuel` = result$fuelUsage2
    )
    
    return (data)
  }
  
  addRouteLines <- function(map) {
    data <- mapData()
    
    print(data)
    
    m <- addPolylines(map, lng = c(data[1,c("Longitude")], data[2,c("Longitude")]),
                           lat = c(data[1,c("Latitude")], data[2,c("Latitude")]),
                           weight = 2)
    
    if (nrow(data) > 2) {
      # Add route lines for both legs
      m <- addPolylines(m, lng = c(data[2,c("Longitude")], data[3,c("Longitude")]),
                           lat = c(data[2,c("Latitude")], data[3,c("Latitude")]),
                           weight = 2)
    }
    
    return (m)
  }
  
  output$routeMap <- renderLeaflet({
    leaflet(data = mapData()) %>% addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, label = ~Location, color = ~Color, stroke = F, radius = 5, weight = 3, fillOpacity = 0.5) %>%
      addRouteLines()
      #addPolylines(lng = ~Longitude, lat = ~Latitude)
  })
  
  outputOption <- function(result) {
    if (is.na(result$mid)) {
      data <- oneLegOptionData(result)
    } else {
      data <- twoLegOptionData(result)
    }
    
    return (div(
      renderDataTable(data, options = list(paging = F, searching = F, info = F)),
      h5(sprintf("Total earnings: $%.0f", result$totalEarnings)),
      h5(sprintf("Total distance: %.0f nm", result$totalDistance)),
      h5(sprintf("Total block time: %.0f minutes", result$totalDuration)),
      leafletOutput("routeMap")
    ))
  }
  
  is.empty <- function(v) {
    if (is.null(v) || nchar(v) < 1) {
      return (T)
    }
    return (F)
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
    input$region
    
    isolate({
      rentalAircraft <- fse.findRentalAircraft(input$aircraft, waterOk = F)
      rentalAircraft <- limitByRegion(rentalAircraft, region())
    })
    
    return (rentalAircraft)
  })
  
  results <- reactive({
    # Dependencies
    input$aircraft
    input$region
    
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
        if (Sys.getenv("TESTRUN") == "true") {
          rentalAircraft <- rentalAircraft[1:(min(5, nrow(rentalAircraft))),]
        }
        minDistance <- input$distance[1]
        maxDistance <- input$distance[2]
        leg1 <- getRankedAssignments(rentalAircraft, 0, maxDistance, progress = function(v, m) {
          setProgress(0.1 + (v * 0.2),
                      message = "Fetching assignments (Leg 1)",
                      detail = sprintf("%.0f / %.0f", v * nrow(rentalAircraft), nrow(rentalAircraft)))
        })
        
        # Fetch leg 2
        if (Sys.getenv("TESTRUN") == "true") {
          leg1 <- leg1[1:(min(10, nrow(leg1))),]
        }
        leg2 <- getRankedAssignments(rentalAircraft, minDistance, maxDistance, leg1$ToIcao, leg1$FromIcao, progress = function(v, m) {
          setProgress(0.3 + (v * 0.6),
                      message = "Fetching assignments (Leg 2",
                      detail = sprintf("%.0f / %.0f", v * nrow(leg1), nrow(leg1)))
        })
        
        # Gather results
        setProgress(0.9, detail = "Gathering results")
        results <- gatherResults(leg1, leg2, maxDistance)
      })
    )
    
    return (results)
  })
  
  currentOption <- reactive({
    results <- results()
    return (results[values$optionNumber,])
  })
  
  numberOfResults <- reactive({
    results <- results()
    return (nrow(results))
  })
  
  mapData <- reactive({
    option <- currentOption()
    
    df <- data.frame(Location = character(), Color = character(), OrderNo = integer(), stringsAsFactors = F)
    if (is.na(option$mid)) {
      df[1,] <- list(Location = option$start, Color = "green", OrderNo = 1)
      df[2,] <- list(Location = option$end, Color = "red", OrderNo = 2)
    } else {
      df[1,] <- list(Location = option$start, Color = "green", OrderNo = 1)
      df[2,] <- list(Location = option$mid, Color = "blue", OrderNo = 2)
      df[3,] <- list(Location = option$end, Color = "red", OrderNo = 3)
    }
    
    df <- merge(df, icaoloc, by = "Location")
    df <- df[order(df$OrderNo),]
    
    return (df)
  })
  
  output$results <- renderUI({
    # Dependencies
    if (is.empty(input$aircraft) || is.empty(input$region)) {
      return()
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
    ac <- fse.getAircraft(input$aircraft)
    if (nrow(ac) < 1) {
      values$durationMin <- Inf
      values$durationMax <- Inf
    } else {
      values$durationMin <- calc.duration(ac, input$distance[1])
      values$durationMax <- calc.duration(ac, input$distance[2])
    }
  })
  
  observeEvent(input$optionBook, {
    withProgress(message = "Booking assignments", value = 0, {
      a <- currentOption()
      
      setProgress(0.1, detail = "Leg 1")
      
      group <- group()
      
      # Book leg 1
      fse.bookAssignments(a$start, a$assignmentIds1, group_id = group$groupId)
      
      if (!is.na(a$mid)) {
        setProgress(0.5, detail = "Leg 2")
        
        # Book leg 2 if there is one
        fse.bookAssignments(a$mid, a$assignmentIds2, group_id = group$groupId)
      }
    })
  })
  
  observeEvent(input$acRent, {
    withProgress(message = "Renting aircraft", value = 0, {
      a <- currentOption()
      
      rentalAircraft <- rentalAircraft()
      ac <- rentalAircraft[rentalAircraft$Location == a$start,]
      if (a$dry1) {
        ac <- ac[order(ac$RentalDry),]
      } else {
        ac <- ac[order(ac$RentalWet),]
      }
      ac <- ac[1,]
      
      setProgress(0.1, detail = "Contacting FSE")
      
      fse.rentAircraft(ac$Location, ac$Registration, a$dry1)
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
