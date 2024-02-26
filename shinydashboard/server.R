server <- function(input, output) {
  
  # filter fish data ----
  filtered_fish_data <- reactive({
    
    fish_data %>%
      filter(AvgDDT >= input$DDT_slider_input[1] & AvgDDT <= input$DDT_slider_input[2])
    
  })
  
  
  # build leaflet map ----
  output$fish_map_output <- renderLeaflet({
    
    leaflet() %>%
      
      # add titles
      addProviderTiles(providers$Esri.WorldImagery) %>%

      
      # set view over CA
      setView(lng = -117.784, lat = 30.0906, zoom = 5) %>%
      
      # add mini map 
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>%
      
      # add markers
      addMarkers(data = filtered_fish_data(),
                 lng = filtered_fish_data()$CompositeTargetLongitude, lat = filtered_fish_data()$CompositeTargetLatitude,
                 popup = paste0("Site Name: ", filtered_fish_data()$CompositeStationArea, "<br>",
                                "Avg DDT: ", filtered_fish_data()$AvgDDT, "<br>"))
  })
  
  # Model testing code
  predict_DDT <- function(species, latitude, longitude) {
    # Placeholder for the actual prediction
    return(runif(1, min = 0, max = 10))
  }
  
  observeEvent(input$predict_button, {
    species <- input$species
    latitude <- input$latitude
    longitude <- input$longitude
    
    prediction <- predict_DDT(species, latitude, longitude)
    
    output$prediction <- renderPrint({
      paste("Predicted DDT Concentration:", round(prediction, 2), "ng/g lipid")
    })
  })
}

  