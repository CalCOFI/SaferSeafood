server <- function(input, output, session) {
  
  
  
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
      setView(lng = -119.784, lat = 30.0906, zoom = 6) %>%
      # add mini map 
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>%
      # add markers
      addMarkers(data = filtered_fish_data(),
                 lng = ~CompositeTargetLongitude, lat = ~CompositeTargetLatitude,
                 popup = ~paste("Site Name: ", CompositeStationArea, "<br>",
                                "Avg DDT: ", AvgDDT, "<br>"))
  })
  
  # Bayesian regression model for prediction
  model <- brm.diet.habitat.year.fam.clean
    
  ## Functions
  
  calculateDDTValue <- function(latitude, longitude) {
    # Placeholder logic to calculate DDT value based on latitude and longitude
    return(0.735932630)  # Placeholder constant value
  }
  
  getYear <- function(Year) {
    # Placeholder logic to get the year
    return(2)  # Placeholder constant value
  }
  
  
  # Prediction function using the Bayesian model
  predict_DDT <- function(species, latitude, longitude) {
    # Determine predictor values based on input
    
    TotalDDT_value = calculateDDTValue(latitude, longitude)  
    Year_value = getYear()  # function or logic
    
    # filer the fish life history dataframe for the species inputted by the user
    input_species <- fish_lh %>% 
      filter(CompositeCommonName %in% species)
    
    # create data frame with values based on user input and life history
    new_data <- data.frame(
      TotalDDT.sed.trans = TotalDDT_value,
      trophic_category = input_species$trophic_category,
      feeding_position = input_species$feeding_position,
      Year = Year_value,
      Family = input_species$Family
    )
    
    # Predict using the model and the new_data
    prediction <- predict(model, newdata = new_data, re.form = NA)
    
    # access the Estimate from the resulting dataframe
    estimate <- prediction[1]
    
    # undo the transformation on the data of log1p()
    estimate_trans <- exp(estimate) - 1
    
    return(estimate_trans) # Adjust based on prediction result structure
  }
  
  # In your server function, when calling predict_DDT, ensure you pass the right arguments:
  observeEvent(input$predict_button, {
    
    species <- input$CompositeCommonName
    latitude <- input$lat 
    longitude <- input$long
    
    # Call the prediction function
    prediction <- predict_DDT(species, latitude, longitude)
    
    
    # Render the prediction in the UI
    output$prediction <- renderPrint({
      paste("Predicted DDT Concentration:", round(prediction, 2), "ng/g lipid")
    })
  })
  
  
  output$lat <- renderPrint({
    input$lat
  })
  
  output$long <- renderPrint({
    input$long
  })
  
  output$geolocation <- renderPrint({
    input$geolocation
  })
}
