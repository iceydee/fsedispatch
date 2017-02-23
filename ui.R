library(shiny)
source("./src/fseconomy.R")

aircrafts <<- fse.getAircraft()

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # # CSS for the progress bar
  # tags$head(
  #   tags$style(HTML("
  #     .shiny-progress {
  #       top: 50% !important;
  #       left: 50% !important;
  #       margin-top: -100px !important;
  #       margin-left: -250px !important;
  #     }
  #   "))
  # ),
  
  # Application title
  headerPanel("FSEDispatch"),
  
  sidebarPanel(
    selectizeInput("aircraft", NULL, c("", aircrafts$MakeModel),
                   options = list(placeholder = "Choose your aircraft")),
    sliderInput("distance", "Target distance:",
                min = 0, max = 2000, value = c(200, 400),
                step = 50, post = " nm"),
    h5(textOutput("duration")),
    uiOutput("regionSelect"),
    uiOutput("groupSelect")
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    uiOutput("results")
  )
))