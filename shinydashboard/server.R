server <- function(input, output, session) {
  
  
  
  # filter fish data ----
  filtered_fish_data <- reactive(
    fish_data
  )
  
  # Initialize current_markers
  current_markers <- reactiveValues(lat = NULL, long = NULL)
  
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
      # swan diego bay and mission bay aren't relevant for coastal advisories 
      
      addPolygons(data= ventura,color = "red",weight = 1,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 0.25,fillColor = "red",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE)) %>% 
      #addPolygons(data= mission,color = "black",weight = 1,smoothFactor = 1,
                  #opacity = 0.5, fillOpacity = 0.25,fillColor = "black",
                  #highlightOptions = highlightOptions(color = "blue",
                                                      #weight = 2,bringToFront = TRUE)) %>%
      addPolygons(data= sbpier,color = "green",weight = 1,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 0.25,fillColor = "green",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE)) %>%
      addPolygons(data= smbeach,color = "hotpink",weight = 1,smoothFactor = 1,
                  opacity = 0.5, fillOpacity = 0.25,fillColor = "hotpink",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2,bringToFront = TRUE))%>%
      #addPolygons(data= sd_bay,color = "orange",weight = 1,smoothFactor = 1,
                  #opacity = 0.5, fillOpacity = 0.25,fillColor = "orange",
                  #highlightOptions = highlightOptions(color = "blue",
                                                      #weight = 2,bringToFront = TRUE)) %>%
      setView(lng = -119.112636297941, lat = 32.7981486713485, zoom = 7) %>%
      # add mini map 
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>% 
      addMarkers(lat = 33.2016761912433,lng = -118.321416825056, 
                 options = markerOptions(draggable = TRUE)) 
      #addCircleMarkers(data = fish_data_clean, 
                 #lng = fish_data_clean$CompositeTargetLongitude, lat = fish_data_clean$CompositeTargetLatitude,
                 #popup = paste0("DDT: ", fish_data_clean$AvgDDT, "<br>",
                               # "Zone: ", fish_data_clean$CompositeStationArea, "<br>"),
                 #color = "white") # NOTE: Unicode for degree symbol icon
  })

  observeEvent(input$locationMap_marker_dragend, {
    # Update current_markers
    current_markers$lat <- input$locationMap_marker_dragend$lat
    current_markers$long <- input$locationMap_marker_dragend$lng
  })  
  
  output$text <- renderText({
    paste0("Current marker latitude: ", current_markers$lat, " <br> ",
           "Current marker longitude: ", current_markers$long, " <br> ")
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
  
  calculateDDT <- function(lat, long){
    
    # construct df out of lat and long inputs 
    lonlat <-data.frame(cbind(long,lat))
    # making the point into a dataframe / sf object
    lonlat_sf = st_as_sf(lonlat, coords=c("long", "lat"), crs="EPSG:4326")
    
    # make the areas df into a spatial object
    polsf <- sf::st_as_sf(areas)
    
    # out_points : user inputted long and lat (location as point)
    # polsf is OEHHA/client polygon 
    # psf are randomly assigned points to check if it's working as it should 
    # ID is fishing zone name 
    
    # assign the point to a fishing zone polygon based on nearest distance
    nearest <- polsf[sf::st_nearest_feature(lonlat_sf, polsf) ,]
    
    # assign point a sediment DDT value
    zone_id <- lonlat_sf %>% 
      mutate(sedDDT = nearest$AvgDDT,
             zone = nearest$Name)
    
    #sedDDT_trans <- exp(zone_id$sedDDT) - 1
    
    # to add into the model we would call:
    TotalSed <- zone_id$sedDDT
    
    # transform the data
    Trans_sed <- log1p(TotalSed)
    
    return(Trans_sed)
    
  }
  
  
  # Function to calculate the DDT value based on latitude and longitude
  #calculateDDTValue <- function(latitude, longitude) {
    # Fit a simple regression model using latitude and longitude to predict TotalDDT.sed.trans
    #lat_lon_model <- lm(TotalDDT.sed.trans ~ CompositeTargetLatitude + CompositeTargetLongitude, data = fish.clean.fam)
    
    # Create a new data frame for the prediction input
    #lat_lon_data <- data.frame(CompositeTargetLatitude = latitude, CompositeTargetLongitude = longitude)
    
    # Predict TotalDDT.sed.trans for the new latitude and longitude
    #predicted_ddt <- predict(lat_lon_model, newdata = lat_lon_data)
    
    #return(predicted_ddt)

  
  
  
  getYear <- function(Year) {
    # Placeholder logic to get the year
    return(2)  # Placeholder constant value
  }
  
  
  # Prediction function using the Bayesian model
  predict_DDT <- function(species, lat, long) {
    # Determine predictor values based on input
    
    TotalDDT_sed_value = calculateDDT(lat, long)  
    Year_value = getYear()  # function or logic
    
    species_name <- tolower(input$species)
    
    # Filter the fish life history dataframe for the species provided as argument
    input_species <- fish_lh %>% 
      filter(CompositeCommonName %in% species_name)
    
    # Check if the filtered data frame is empty and handle appropriately
    if (nrow(input_species) == 0) {
      # Change depending on what we want to output
      return(NA)
    }
    
    new_data <- data.frame(
      TotalDDT.sed.trans = TotalDDT_sed_value,
      trophic_category = input_species$trophic_category,
      feeding_position = input_species$feeding_position,
      Year = Year_value,
      Family = input_species$Family
    )
    
    # Check if all necessary data is available
    if (anyNA(new_data)) {
      # Change depending on what we want to output
      return(NA)
    }
    
    prediction <- predict(model, newdata = new_data, re.form = NA)
    estimate <- prediction[1]
    estimate_trans <- exp(estimate) - 1
    
    return(estimate_trans)
  }
  
  
    updateSelectizeInput(session, 'CompositeCommonName', choices = species_name_clean, server = TRUE)
  
  
  # In your server function, when calling predict_DDT, ensure you pass the right arguments:
  observeEvent(input$predict_button, {
    
    species <- input$CompositeCommonName
    latitude <- current_markers$lat  # Use current latitude
    longitude <- current_markers$long  # Use current longitude
    
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

