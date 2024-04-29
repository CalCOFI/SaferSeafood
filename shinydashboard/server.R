server <- function(input, output, session) {
  
  
  
  # filter fish data ----
  filtered_fish_data <- reactive(
    fish_data
  )
  
  # Initialize current_markers
  current_markers <- reactiveValues(lat = NULL, lon = NULL)
  
  # build location selectionleaflet map ----
  output$locationMap <- renderLeaflet({
    
    #kml <- st_read("/Users/katebecker/Documents/Bren/Capstone/shiny-map-dash/shinydashboard/data/polygons/venturaharbor.kml") %>%
      
      #leaflet_data <- sf::as.data.frame(kml) 
      
    leaflet() %>%
      # add titles
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% # added new map for visualize import streets
      #addKML("/Users/katebecker/Documents/Bren/Capstone/shiny-map-dash/shinydashboard/data/polygons/smbeach_to_sb.kml") %>%
      # set view over CA 
      # Check if sf_data contains polygons
      
      addPolygons(data= ventura,color = "red",weight = 1,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 0.25,fillColor = "red",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE)) %>% 
      addPolygons(data= mission,color = "black",weight = 1,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 0.25,fillColor = "black",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE)) %>%
      addPolygons(data= sbpier,color = "green",weight = 1,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 0.25,fillColor = "green",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE)) %>%
      addPolygons(data= smbeach,color = "hotpink",weight = 1,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 0.25,fillColor = "hotpink",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      addPolygons(data= sd_bay,color = "purple",weight = 1,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 0.25,fillColor = "purple",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE)) %>%
      setView(lng = -119.112636297941, lat = 32.7981486713485, zoom = 7) %>%
      # add mini map 
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>% 
      addMarkers(lat = 33.2016761912433,lng = -118.321416825056, 
                 options = markerOptions(draggable = TRUE)) %>%
      addCircleMarkers(data = fish_data_clean, 
                 lng = fish_data_clean$CompositeTargetLongitude, lat = fish_data_clean$CompositeTargetLatitude,
                 popup = paste0("DDT: ", fish_data_clean$AvgDDT, "<br>",
                                "Zone: ", fish_data_clean$CompositeStationArea, "<br>"),
                 color = "white") # NOTE: Unicode for degree symbol icon
  })

  observeEvent(input$locationMap_marker_dragend, {
    # Update current_markers
    current_markers$lat <- input$locationMap_marker_dragend$lat
    current_markers$lon <- input$locationMap_marker_dragend$lng
  })  
  
  output$text <- renderText({
    paste0("Current marker latitude: ", current_markers$lat, " <br> ",
           "Current marker longitude: ", current_markers$lon, " <br> ")
  })
  
  
  # build data exploration leaflet map ----
  output$fish_map_output <- renderLeaflet({
    leaflet() %>%
      # add titles
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addPolygons(data = ventura, fillColor = "red", color = "black", weight = 2, opacity = 1) %>%
      # set view over CA
      setView(lng = -119.784, lat = 30.0906, zoom = 6) %>%
      # add mini map 
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>%
      # add markers
      addMarkers(data = filtered_fish_data(),
                 lng = ~CompositeTargetLongitude, lat = ~CompositeTargetLatitude,
                 popup = ~paste("Site Name: ", CompositeStationArea, "<br>",
                                "Avg DDT: ", AvgDDT, "<br>"))
    
    # observeEvent(input$click, {
    #   
    #   session$sendCustomMessage("onMapClick", click)
    #   
    # })
    
    # observeEvent(input$map_marker_click, {
    #   data$clickedMarker <- input$map_marker_click
    #   print(data$clickedMarker)
    # })
    # observeEvent(input$map_click, {
    #   data$clickedMarker <- NULL
    #   print(data$clickedMarker)
    # })

    
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
      filter(CompositeCommonName %in% str_to_lower(input$selectSpecies_input))
    
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
  
  updateSelectizeInput(session, 'CompositeCommonName', choices = species_name_clean, server = TRUE)
  
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
  
  # observeEvent(input$location_marker_click, {
  #   js$backgroundCol(input$selector, input$col)
  # })
  
  # Use clicked loction on map to find lat and long
  # data_of_click <- reactiveValues(clickedMarker = list())
  
  # observeEvent(input$my_location,{
  #   #Only add new layers for bounded locations
  #   found_in_bounds <- findLocations(shape = input$my_location
  #                                    , location_coordinates = coordinates
  #                                    , location_id_colname = "locationID")
  #   
  #   for(id in found_in_bounds){
  #     if(id %in% data_of_click$clickedMarker){
  #       # don't add id
  #     } else {
  #       # add id
  #       data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
  #     }
  #   }
  # })
}
