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
              min = 0, max = 7240, value = c(0, 7200)),
  
  # body mass plot output ----
  plotOutput(outputId = "DDT_concentration_scatter_output")

  )

# server instructions ----
server <- function(input, output) {
  
  #filter DDT concentrations 
  DDT_df <- reactive({
    southern_fish %>%
    filter(TotalDDT %in% c(input$DDT_conc_input[1]:input$DDT_conc_input[2]))
  })
  
  #render ltered DDT scatter plot reactive ----
  output$DDT_concentration_scatter_output <- renderPlot({
    ggplot(na.omit(DDT_df()), 
           aes(x = TotalDDT, y = WeightAvg.g)) +
      geom_point(color = "red") +
      theme_minimal() +
      theme(legend.position = c(0.85, 0.2),
            legend.background = element_rect(color = "white"))
    
  })
  
}

# combine UI & server into an app ----
shinyApp(ui = ui, server = server)