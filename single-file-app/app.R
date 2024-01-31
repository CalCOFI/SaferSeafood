# load packages ----
library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(markdown)
library(rsconnect)

# user interface ----
ui <- fluidPage(
  # app title ----
  tags$h1("SaferSeafood"),
  
  # app subtitle ----
  h4(strong("Landing page for DDT concentration analysis")),
  
  # total DDT slider input ----
  sliderInput(inputId = "DDT_conc_input", label = "Select a DDT range :",
              min = 0, max = 7240, value = c(10, 7200))
)

# server instructions ----
server <- function(input, output) {}

# combine UI & server into an app ----
shinyApp(ui = ui, server = server)