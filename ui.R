library(shiny)
source("./src/fseconomy.R")

aircrafts <<- fse.getAircraft()

# Define UI for miles per gallon application
shinyUI(fluidPage(
  
  # CSS for the progress bar
  tags$head(
   tags$style(HTML("
     #shiny-notification-panel {
       top: 50% !important;
       left: 50% !important;
       margin-top: -100px !important;
       margin-left: -125px !important;
     }
   "))
  ),
  
  # Application title
  headerPanel("FSEDispatch"),
  
  sidebarPanel(
    selectizeInput("aircraft", NULL, c("", aircrafts$MakeModel),
                   options = list(placeholder = "Choose your aircraft")),
    checkboxInput("airline", "Airline assignment", value = F),
    sliderInput("distance", "Target distance:",
                min = 0, max = 2000, value = c(200, 400),
                step = 50, post = " nm"),
    sliderInput("hops", "Max hops:",
                min = 1, max = 3, value = 2, step = 1),
    h5(textOutput("duration")),
    # TODO: This is for when we have own aircraft
    # selectizeInput("thirdLeg", NULL, c("", "Profit", "Destination"),
    #                options = list(placeholder = "Select onward journey bias")),
    # uiOutput("destinationSelect"),
    # sliderInput("onwardWeight", "Onward journey weighting:",
    #             min = 0.0, max = 2.0, value = 0.5,
    #             step = 0.1),
    uiOutput("regionSelect"),
    uiOutput("groupSelect")
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    uiOutput("results")
  )
))