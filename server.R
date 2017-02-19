library(shiny)
library(datasets)
source("./src/regions.R")
source("./src/fseconomy.R")
source("./src/earnings.R")
source("./src/icao.R")

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  durationText <- reactive({
    ac <- fse.getAircraft(input$aircraft)
    sprintf("Estimated duration %.2fh - %.2fh",
            calc.duration(ac, input$distance[1]),
            calc.duration(ac, input$distance[2]))
  })
  
  output$duration <- renderText({
    durationText()
  })
  
  icaoOutput <- function(icao) {
    x <- icao.get(icao)
    return (sprintf("%s (%s, %s)", x$icao, x$name, x$country))
  }
  
  output$regionSelect <- renderUI({
    if (nchar(input$aircraft) < 1) {
      return()
    }
    rentalAircraft <- fse.findRentalAircraft(
      input$aircraft,
      waterOk = FALSE
    )
    
    # Check how many ac we have in each region
    regions$count <- sapply(1:nrow(regions), function (n) {
      return (nrow(limitByRegion(rentalAircraft, regions[n,])))
    })
    
    regions <- regions[regions$count > 0,]
    
    r <- sapply(1:nrow(regions), function(n) {
      sprintf("%s (%i aircraft)", regions$name[n], regions$count[n])
    })
    selectizeInput("region", NULL, c("", r),
                   options = list(placeholder = "Select a region"))
  })
  
  oneLegOptionData <- function(result) {
    data <- list(From = character(),
                       To = character(),
                       Distance = integer(),
                       Amount = integer(),
                       Commodity = character(),
                       Duration = double(),
                       `Minimum Fuel` = integer(),
                       stringsAsFactors = F)
    data[1,] <- data.frame(
      From = icaoOutput(result$start),
      To = icaoOutput(result$end),
      Distance = result$distance1,
      Amount = result$amount1,
      Commodity = result$commodity1,
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
                       Duration = double(),
                       `Minimum Fuel` = integer(),
                       stringsAsFactors = F)
    data[1,] <- list(
      From = icaoOutput(result$start),
      To = icaoOutput(result$mid),
      Distance = result$distance1,
      Amount = result$amount1,
      Commodity = result$commodity1,
      Duration = result$duration1,
      `Minimum Fuel` = result$fuelUsage1
    )
    data[2,] <- list(
      From = icaoOutput(result$mid),
      To = icaoOutput(result$end),
      Distance = result$distance2,
      Amount = result$amount2,
      Commodity = result$commodity2,
      Duration = result$duration2,
      `Minimum Fuel` = result$fuelUsage2
    )
    
    return (data)
  }
  
  outputOption <- function(result) {
    if (is.na(result$mid)) {
      data <- oneLegOptionData(result)
    } else {
      data <- twoLegOptionData(result)
    }
    return (div(
      renderDataTable(data),
      h5(sprintf("Total earnings: $%.0f", result$totalEarnings)),
      h5(sprintf("Total distance: %i nm", result$totalDistance)),
      h5(sprintf("Total duration: %.2fh", result$totalDuration))
    ))
  }
  
  is.empty <- function(v) {
    if (is.null(v) || nchar(v) < 1) {
      return (T)
    }
    return (F)
  }
  
  output$results <- renderUI({
    if (is.empty(input$aircraft) || is.empty(input$region)) {
      return()
    }
    
    withProgress(message = "Finding assignments", value = 0, {
      # Find rental aircraft
      incProgress(0.1, detail = "Finding rental aircraft")
      region <- sub(" \\([0-9]+ aircraft\\)", "", input$region)
      region <- regions[regions$name == region,]
      rentalAircraft <- fse.findRentalAircraft(input$aircraft, waterOk = F)
      rentalAircraft <- limitByRegion(rentalAircraft, region)
      
      # Fetch leg 1
      incProgress(0.2, detail = "Fetching assignments for leg 1")
      minDistance <- input$distance[1]
      maxDistance <- input$distance[2]
      leg1 <- getRankedAssignments(rentalAircraft, 0, maxDistance)
      
      # Fetch leg 2
      incProgress(0.3, detail = "Fetching assignments for leg 2")
      leg2 <- getRankedAssignments(rentalAircraft, minDistance, maxDistance, leg1$ToIcao, leg1$FromIcao)
      
      # Gather results
      incProgress(0.9, detail = "Gathering results")
      results <- gatherResults(leg1, leg2, maxDistance)
    })
    
    div(
      h2("Results are in"),
      h3("Option 1"),
      outputOption(results[1,]),
      h3("Option 2"),
      outputOption(results[2,])
    )
  })
})
