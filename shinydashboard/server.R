server <- function(input, output, session) {
  
  
  
  # filter fish data ----
  filtered_fish_data <- reactive({
    fish_data %>%
      filter(AvgDDT >= input$DDT_slider_input[1] & AvgDDT <= input$DDT_slider_input[2])
  })
  
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
  
  
  # Bayesian regression model for prediction
  model <- brm.diet.habitat.year.fam.clean
  

  ## Functions
  
  calculateDDTValue <- function(latitude, longitude) {
    # Placeholder logic to calculate DDT value based on latitude and longitude
    return(0.735932630)  # Placeholder constant value
  }
  
  determineTrophicCategory <- function(species) {
    # Placeholder logic to determine trophic category based on species
    return("Secondary Carnivore")  # Placeholder constant value
  }
  
  determineFeedingPosition <- function(species) {
    # Placeholder logic to determine feeding position based on species
    return("Benthopelagic")  # Placeholder constant value
  }
  
  getYear <- function(Year) {
    # Placeholder logic to get the year
    return(2)  # Placeholder constant value
  }
  
  determineFamily <- function(species) {
    # Placeholder logic to determine family based on species
    return("Sciaenidae")  # Placeholder constant value
  }
  
  
  # Prediction function using the Bayesian model
  predict_DDT <- function(species, latitude, longitude) {
    # Determine predictor values based on input
    
    TotalDDT_value = calculateDDTValue(latitude, longitude)  
    trophic_category_value = determineTrophicCategory(species) 
    feeding_position_value = determineFeedingPosition(species) 
    Year_value = getYear()  # function or logic
    Family_value = determineFamily(species) 
    
    new_data <- data.frame(
      TotalDDT.sed.trans = TotalDDT_value,
      trophic_category = trophic_category_value,
      feeding_position = feeding_position_value,
      Year = Year_value,
      Family = Family_value
    )
    
    # Predict using the model and the new_data
    prediction <- predict(model, newdata = new_data, re.form = NA)
    
    return(prediction) # Adjust based on prediction result structure
  }
  
  
  # In your server function, when calling predict_DDT, ensure you pass the right arguments:
  observeEvent(input$predict_button, {
    species <- input$CompositeCommonName
    latitude <- input$CompositeTargetLatitude
    longitude <- input$CompositeTargetLongitude
    
    # Call the prediction function
    prediction <- predict_DDT(species, latitude, longitude)
    
    # access the Estimate from the resulting dataframe
    estimate <- prediction[1]
    
    # Render the prediction in the UI
    output$prediction <- renderPrint({
      paste("Predicted DDT Concentration:", round(prediction[1], 2), "ng/g lipid")
    })
  })
}
