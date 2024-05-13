server <- function(input, output, session) {
  
  
  # filter fish data ----
  filtered_fish_data <- reactive(
    fish_data
  )
  
  # Initialize current_markers
  current_markers <- reactiveValues(lat = 33.726973, long = -118.377620, zoom = 10)
  
  # build location selectionleaflet map ----
  output$locationMap <- renderLeaflet({
    
    #kml <- st_read("/Users/katebecker/Documents/Bren/Capstone/shiny-map-dash/shinydashboard/data/polygons/venturaharbor.kml") %>%
    
    #leaflet_data <- sf::as.data.frame(kml) 
    
    leaflet() %>%
      # add titles
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% # added new map for visualize import streets
      
      addPolygons(data = shelf, color = "darkorange") %>%
      
      #addRasterImage(rstack, opacity = 0.5) %>%
      
      # addPolygons(data = rstack, fillColor = "blue", color = "black", weight = 1, opacity = 1) %>% 
      
      #set view over CA
      #Check if sf_data contains polygons
      #swan diego bay and mission bay aren't relevant for coastal advisories
      
      addPolygons(data= ventura, color = "yellow",weight = 3,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.25,fillColor = "transparent", dashArray = "5, 5", 
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2)) %>%
      
      addPolygons(data= sbpier,color = "yellow",weight = 3,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.25,fillColor = "transparent",
                  dashArray = "5, 5",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2)) %>%
      
      addPolygons(data= smbeach,color = "yellow",weight = 3,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.25,fillColor = "transparent",
                  dashArray = "5, 5",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2)) %>% 
      
      addLegend(values = NULL,
                title = '<small>Areas of Interest</small>',
                position = "topleft",
                colors = c("darkorange", "yellow", "red"),
                labels = c("Palos Verdes Shelf", "Study Area", "Barrel field of DDT-laced sludge")) %>%
      
      setView(lng = -118.377620, lat = 33.726973, zoom = 9) %>%
      # add mini map 
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>% 
      addMarkers(lat = 33.726973,lng = -118.377620,
                 options = markerOptions(draggable = TRUE)) %>%
      addCircleMarkers(lng = -118.48, lat = 33.55, color = "red",
                       radius = 20)
    })

  observeEvent(input$locationMap_marker_dragend, {
    # Update current_markers
    current_markers$lat <- input$locationMap_marker_dragend$lat
    current_markers$long <- input$locationMap_marker_dragend$lng
    
  })  
  
  
  #advisory function 
  get_advisory <- function(lat, long) {
    
    # construct df out of lat and long inputs 
    lonlat <- data.frame(cbind(long,lat))
    # making the point into a dataframe / sf object
    lonlat_sf = st_as_sf(lonlat, coords=c("long", "lat"), crs="EPSG:4326")
    
    # make the areas df into a spatial object
    polsf <- sf::st_as_sf(advisory_areas)
    
    # assign the point to a fishing zone polygon based on nearest distance
    nearest <- polsf[sf::st_nearest_feature(lonlat_sf, polsf) ,]
    
    # assign point a sediment DDT value
    advisory_id <- lonlat_sf %>% 
      mutate(name = nearest$Name) %>% 
      st_drop_geometry()
    
    name <- advisory_id[[1]]
    
    return(name)
    
  }
  
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
    
    # assign the point to a fishing zone polygon based on nearest distance
    nearest <- polsf[sf::st_nearest_feature(lonlat_sf, polsf) ,]
    
    
    # assign point a sediment DDT value
    zone_id <- lonlat_sf %>%
      mutate(sedDDT = nearest$AvgDDT,
             zone = nearest$Name)
    
    # to add into the model we would call:
    TotalSed <- zone_id$sedDDT
    
    # transform the data
    Trans_sed <- log1p(TotalSed)
    
    return(Trans_sed)
    
  }
  
  getYear <- function(Year) {
    # Placeholder logic to get the year
    return(2)  # Placeholder constant value
  }
  
  
  # Prediction function using the Bayesian model
  predict_DDT <- function(species, lat, long) {
    # Determine predictor values based on input
    
    # add in the current markers for DDT
    TotalDDT_sed_value = calculateDDT(lat = current_markers$lat, 
                                      long = current_markers$long)  
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
    
    # Clear the input field
    # updateTextInput(session, inputId = "species", value = "")
    
    species <- input$CompositeCommonName
    latitude <- current_markers$lat  # Use current latitude
    longitude <- current_markers$long  # Use current longitude
    advisory_name <- get_advisory(lat = current_markers$lat, 
                                  long = current_markers$long)
    
    path = "data/OEHHA/"
    species_name_advisory <- input$species
    species_name_img <- tolower(gsub(" ", "-", input$species))
    
    #Determine image path based on advisory name
    image_path <- read_csv(paste0(path, advisory_name, "/other_advisory.csv")) %>% 
      filter(Species == species_name_advisory)
    
    # Call the prediction function
    prediction <- predict_DDT(species, latitude, longitude)
    
    # create assignment for serving size based on prediction value
    assignment_of_serving <- data.frame(pred = prediction) %>% 
      mutate(rec = ifelse(prediction <= 21,
                          "Safe",
                          ifelse(prediction > 21 & prediction <= 220,
                                 7,
                                 ifelse(prediction > 220 & prediction <= 260,
                                        6,
                                        ifelse(prediction > 260 & prediction <= 310,
                                               5, 
                                               ifelse(prediction > 310 & prediction <= 390,
                                                      4, 
                                                      ifelse(prediction > 390 & prediction <= 520,
                                                             3,
                                                             ifelse(prediction > 520 & prediction <= 1000,
                                                                    2,
                                                                    ifelse(prediction > 1000 & prediction <= 2100,
                                                                           1,
                                                                           ifelse(prediction > 2100,
                                                                                  "Do Not Consume",
                                                                                  NA))))))))))
    
    # Extract the value from the data frame
    serving_size <- as.character(assignment_of_serving[1, 2])
    
    # construct df out of lat and long inputs 
    lonlat <- data.frame(cbind(long = longitude,lat = latitude))
    # making the point into a dataframe / sf object
    lonlat_sf = st_as_sf(lonlat, coords=c("long", "lat"), crs="EPSG:4326")
    # make the areas df into a spatial object
    polsf <- sf::st_as_sf(areas)
    # assign the point to a fishing zone polygon based on nearest distance
    nearest <- polsf[sf::st_nearest_feature(lonlat_sf, polsf) ,]
    # get the distance measurement from the point to the nearest polygon
    meters <- st_distance(x = lonlat_sf, y = nearest) %>% 
      as.numeric()
    
    
    ###------------The Output----------------###
    
    # check if the location is valid
    if (meters > 500) {
      output$prediction <- renderText({ NULL })
      output$serving_size <- renderText({ NULL })
      output$advisory <- renderText({ NULL })
      output$validation_result <- renderText({
        "Invalid location selected. Location outside of area of study, please select a different location and try again."
      })
      
    } else {
      
      # Clear the validation_result output if the location is valid
      output$validation_result <- renderText({
        NULL
      })
      # Check if the 'prediction' value is missing (NA)
      if (is.na(prediction)) {
        # Handle the case where prediction is NA by providing an informative message
        output$prediction <- renderText({ NULL })
        output$prediction <- renderText({
          "Prediction not available. Please select a fish species."
        })
        # Clear any previous error messages
        output$serving_size <- renderText({ NULL })
      } else {
        # If prediction is available, render the predicted value in the format of ng/g lipid
        output$prediction <- renderText({ NULL })
        output$prediction <- renderText({
          paste("There are ", round(prediction, 2), "ng of DDT per gram of ", species_name_advisory, ".")
        })
        
        # Display the recommended serving size using the value from 'serving_size'
        output$serving_size <- renderText({
          paste("Based on these results, the recommended serving size for this fish at this location is ",serving_size, " per week. For information about serving size click the info button above.")
        })
        
        # Check if the image associated with the current prediction exists
        # Check if the species exists in the column
        if (nrow(image_path) > 0) {  # Check if there are any image paths found
          # If species found, display the number of servings
          if (length(image_path) >= 3) {  # Check if there are values for both age groups
            if (image_path[[2]] == image_path[[3]]) {  # Check if serving sizes for both age groups are equal
              output$advisory <- renderText({  # Render the advisory message
                paste("The recommended serving size for all age groups is ", image_path[[2]], ".")
              })
            } else {  # If serving sizes for both age groups are different
              output$advisory <- renderText({  # Render the advisory message
                paste("The recommended serving size for women 18-49 years and children 1-17 years is ", image_path[[2]], ". The recommended serving size for women 50 years and older and men 18 years and older is ", image_path[[3]], ".")
              })
            }
          } else {  # If only one serving size is found
            output$advisory <- renderText({  # Render the advisory message
              paste("The recommended serving size for all age and gender groups is ", image_path[[2]], ".")
            })
          }
        } else {  # If no image paths are found
          # If species not found, display "no advisories found"
          output$advisory <- renderText({  # Render the advisory message
            HTML("No other advisories found.")
          })
        }
        
      }
    }
    
    
    
  })
  
  observeEvent(input$show_info_message, {
    showNotification(
      input$show_info_message,
      duration = 5000,
      type = "message"
    )
  })
  
}



















server <- function(input, output, session) {
  
  
  # filter fish data ----
  filtered_fish_data <- reactive(
    fish_data
  )
  
  # Initialize current_markers
  current_markers <- reactiveValues(lat = 33.726973, long = -118.377620, zoom = 10)
  
  # build location selectionleaflet map ----
  output$locationMap <- renderLeaflet({
    
    #kml <- st_read("/Users/katebecker/Documents/Bren/Capstone/shiny-map-dash/shinydashboard/data/polygons/venturaharbor.kml") %>%
    
    #leaflet_data <- sf::as.data.frame(kml) 
    
    leaflet() %>%
      # add titles
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% # added new map for visualize import streets
      
      addPolygons(data = shelf, color = "darkorange") %>%
      
      #addRasterImage(rstack, opacity = 0.5) %>%
      
      # addPolygons(data = rstack, fillColor = "blue", color = "black", weight = 1, opacity = 1) %>% 
      
      #set view over CA
      #Check if sf_data contains polygons
      #swan diego bay and mission bay aren't relevant for coastal advisories
      
      addPolygons(data= ventura, color = "white",weight = 3,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.25,fillColor = "transparent", dashArray = "5, 5", 
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2)) %>%
      
      addPolygons(data= sbpier,color = "white",weight = 3,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.25,fillColor = "transparent",
                  dashArray = "5, 5",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2)) %>%
      
      addPolygons(data= smbeach,color = "white",weight = 3,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.25,fillColor = "transparent",
                  dashArray = "5, 5",
                  highlightOptions = highlightOptions(color = "blue",
                                                      weight = 2)) %>% 
      
      addLegend(values = NULL,
                title = '<small>Areas of Interest</small>',
                position = 'topright',
                colors = c("darkorange", "white", "red"),
                labels = c("Palos Verdes Shelf", "Study Area", "Barrel field of DDT-laced sludge")) %>%
      
      setView(lng = -118.377620, lat = 33.726973, zoom = 9) %>%
      # add mini map 
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>% 
      addMarkers(lat = 33.726973,lng = -118.377620,
                 options = markerOptions(draggable = TRUE)) %>%
      addCircleMarkers(lng = -118.48, lat = 33.55, color = "red",
                       radius = 20)
    })

  observeEvent(input$locationMap_marker_dragend, {
    # Update current_markers
    current_markers$lat <- input$locationMap_marker_dragend$lat
    current_markers$long <- input$locationMap_marker_dragend$lng
    
  })  
  
  
  #advisory function 
  get_advisory <- function(lat, long) {
    
    # construct df out of lat and long inputs 
    lonlat <- data.frame(cbind(long,lat))
    # making the point into a dataframe / sf object
    lonlat_sf = st_as_sf(lonlat, coords=c("long", "lat"), crs="EPSG:4326")
    
    # make the areas df into a spatial object
    polsf <- sf::st_as_sf(advisory_areas)
    
    # assign the point to a fishing zone polygon based on nearest distance
    nearest <- polsf[sf::st_nearest_feature(lonlat_sf, polsf) ,]
    
    # assign point a sediment DDT value
    advisory_id <- lonlat_sf %>% 
      mutate(name = nearest$Name) %>% 
      st_drop_geometry()
    
    name <- advisory_id[[1]]
    
    return(name)
    
  }
  
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
    
    # assign the point to a fishing zone polygon based on nearest distance
    nearest <- polsf[sf::st_nearest_feature(lonlat_sf, polsf) ,]
    
    
    # assign point a sediment DDT value
    zone_id <- lonlat_sf %>%
      mutate(sedDDT = nearest$AvgDDT,
             zone = nearest$Name)
    
    # to add into the model we would call:
    TotalSed <- zone_id$sedDDT
    
    # transform the data
    Trans_sed <- log1p(TotalSed)
    
    return(Trans_sed)
    
  }
  
  getYear <- function(Year) {
    # Placeholder logic to get the year
    return(2)  # Placeholder constant value
  }
  
  
  # Prediction function using the Bayesian model
  predict_DDT <- function(species, lat, long) {
    # Determine predictor values based on input
    
    # add in the current markers for DDT
    TotalDDT_sed_value = calculateDDT(lat = current_markers$lat, 
                                      long = current_markers$long)  
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
    
    # Clear the input field
    # updateTextInput(session, inputId = "species", value = "")
    
    species <- input$CompositeCommonName
    latitude <- current_markers$lat  # Use current latitude
    longitude <- current_markers$long  # Use current longitude
    advisory_name <- get_advisory(lat = current_markers$lat, 
                                  long = current_markers$long)
    
    path = "data/OEHHA/"
    species_name_advisory <- input$species
    species_name_img <- tolower(gsub(" ", "-", input$species))
    
    #Determine image path based on advisory name
    image_path <- read_csv(paste0(path, advisory_name, "/other_advisory.csv")) %>% 
      filter(Species == species_name_advisory)
    
    path2 = "data/fish_image/"
    species_img <- tolower(gsub(" ", "-", input$species))
    
    #Determine image path based on advisory name
    image_path2 <- paste0(path2, species_img, ".png")
    
    # Call the prediction function
    prediction <- predict_DDT(species, latitude, longitude)
    
    # create assignment for serving size based on prediction value
    assignment_of_serving <- data.frame(pred = prediction) %>% 
      mutate(rec = ifelse(prediction <= 21,
                          "Safe",
                          ifelse(prediction > 21 & prediction <= 220,
                                 7,
                                 ifelse(prediction > 220 & prediction <= 260,
                                        6,
                                        ifelse(prediction > 260 & prediction <= 310,
                                               5, 
                                               ifelse(prediction > 310 & prediction <= 390,
                                                      4, 
                                                      ifelse(prediction > 390 & prediction <= 520,
                                                             3,
                                                             ifelse(prediction > 520 & prediction <= 1000,
                                                                    2,
                                                                    ifelse(prediction > 1000 & prediction <= 2100,
                                                                           1,
                                                                           ifelse(prediction > 2100,
                                                                                  "Do Not Consume",
                                                                                  NA))))))))))
    
    # Extract the value from the data frame
    serving_size <- as.character(assignment_of_serving[1, 2])
    
    # construct df out of lat and long inputs 
    lonlat <- data.frame(cbind(long = longitude,lat = latitude))
    # making the point into a dataframe / sf object
    lonlat_sf = st_as_sf(lonlat, coords=c("long", "lat"), crs="EPSG:4326")
    # make the areas df into a spatial object
    polsf <- sf::st_as_sf(areas)
    # assign the point to a fishing zone polygon based on nearest distance
    nearest <- polsf[sf::st_nearest_feature(lonlat_sf, polsf) ,]
    # get the distance measurement from the point to the nearest polygon
    meters <- st_distance(x = lonlat_sf, y = nearest) %>% 
      as.numeric()
    
    
    ###------------The Output----------------###
    
    # check if the location is valid
    if (meters > 500) {
      output$prediction <- renderText({ NULL })
      output$serving_size <- renderText({ NULL })
      output$advisory <- renderText({ NULL })
      output$validation_result <- renderText({
        "Invalid location selected. Location outside of area of study, please select a different location and try again."
      })
      
    } else {
      
      # Clear the validation_result output if the location is valid
      output$validation_result <- renderText({
        NULL
      })
      # Check if the 'prediction' value is missing (NA)
      if (is.na(prediction)) {
        # Handle the case where prediction is NA by providing an informative message
        output$prediction <- renderText({ NULL })
        output$prediction <- renderText({
          "Prediction not available. Please select a fish species."
        })
        # Clear any previous error messages
        output$serving_size <- renderText({ NULL })
      } else {
        # If prediction is available, render the predicted value in the format of ng/g lipid
        output$prediction <- renderText({ NULL })
        output$prediction <- renderText({
          paste("There are ", round(prediction, 2), "ng of DDT per gram of ", species_name_advisory, ".")
        })
        
                # Display the recommended serving size using the value from 'serving_size'
        output$serving_size <- renderText({
          paste("Based on these results, the recommended serving size for this fish at this location is ",serving_size, " per week. For information about serving size click the info button above.")
        })
        output$fish_image <- renderImage({
          if (!file.exists(image_path2)) {
            return(NULL)
          } else {
            return(list(src = image_path2, contentType = "image/png", alt = "Fish Image", width = "200px", height = "100px"))
          }
        }, deleteFile = FALSE)
                # Check if the image associated with the current prediction exists
        # Check if the species exists in the column
        if (nrow(image_path) > 0) {  # Check if there are any image paths found
          # If species found, display the number of servings
          if (length(image_path) >= 3) {  # Check if there are values for both age groups
            if (image_path[[2]] == image_path[[3]]) {  # Check if serving sizes for both age groups are equal
              output$advisory <- renderText({  # Render the advisory message
                paste("The recommended serving size for all age groups is ", image_path[[2]], ".")
              })
            } else {  # If serving sizes for both age groups are different
              output$advisory <- renderText({  # Render the advisory message
                paste("The recommended serving size for women 18-49 years and children 1-17 years is ", image_path[[2]], ". The recommended serving size for women 50 years and older and men 18 years and older is ", image_path[[3]], ".")
              })
            }
          } else {  # If only one serving size is found
            output$advisory <- renderText({  # Render the advisory message
              paste("The recommended serving size for all age and gender groups is ", image_path[[2]], ".")
            })
          }
        } else {  # If no image paths are found
          # If species not found, display "no advisories found"
          output$advisory <- renderText({  # Render the advisory message
            HTML("No other advisories found.")
          })
        }
        
      }
    }
  
    
  })
  
  observeEvent(input$show_info_message, {
    showNotification(
      input$show_info_message,
      duration = 5000,
      type = "message"
    )
  })
  
}



















