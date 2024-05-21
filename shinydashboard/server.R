# Server function
server <- function(input, output, session) {
  
  ### Reactive Values & Data Preparation ###--------------
  
  filtered_fish_data <- reactive(fish_data) # Reactive expression for filtering fish data
  current_markers <- reactiveValues(lat = 33.726973, long = -118.377620, zoom = 10) # Reactive values for current marker position and zoom level
  
  ### Utility Functions ###------------
  
  #advisory function 
  get_advisory <- function(lat, long) {
    # Function to get the advisory area based on latitude and longitude
    
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
  model <- brm.diet.habitat.year.fam.clean # Load the Bayesian regression model
  
  calculateDDT <- function(lat, long){
    # Function to calculate DDT value based on latitude and longitude
    
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
    # Function to predict DDT value using the Bayesian model
    
    # Determine predictor values based on input
    
    # add in the current markers for DDT
    TotalDDT_sed_value = calculateDDT(lat = current_markers$lat, 
                                      long = current_markers$long)  
    Year_value = getYear()  # function or logic
    
    species_name <- tolower(input$species) # Convert species name to lowercase
    
    # Filter the fish life history dataframe for the species provided as argument
    input_species <- fish_lh %>% 
      filter(CompositeCommonName %in% species_name)
    
    # Check if the filtered data frame is empty and handle appropriately
    if (nrow(input_species) == 0) {
      # Change depending on what we want to output
      return(NA)
    }
    
    new_data <- data.frame(
      TotalDDT.sed.trans = TotalDDT_sed_value, # Transformed DDT value
      trophic_category = input_species$trophic_category, # Trophic category of the species
      feeding_position = input_species$feeding_position, # Feeding position of the species
      Year = Year_value, # Year value
      Family = input_species$Family # Family of the species
    )
    
    # Check if all necessary data is available
    if (anyNA(new_data)) {
      # Change depending on what we want to output
      return(NA)
    }
    
    prediction <- predict(model, newdata = new_data, re.form = NA) # Predict using the Bayesian model
    estimate <- prediction[1]
    estimate_trans <- exp(estimate) - 1 # Transform the estimate
    
    return(estimate_trans)
  }
  
  ### Leaflet Map Rendering ###------------------
  
  output$locationMap <- renderLeaflet({
    # Code for rendering the Leaflet map
    
    leaflet() %>%
      # add titles
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% # Add map tiles
      
      addPolygons(data = shelf, color = "darkblue", popup = "Palos Verdes Superfund Site") %>% # Add polygons for Palos Verdes Shelf
      
      addPolygons(data= ventura, color = "white",weight = 3,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.25,fillColor = "transparent", dashArray = "5, 5") %>% # Add polygons for Ventura
      
      addPolygons(data= sbpier,color = "white",weight = 3,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.25,fillColor = "transparent",
                  dashArray = "5, 5") %>% # Add polygons for Santa Barbara Pier
      
      addPolygons(data= smbeach,color = "white",weight = 3,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.25,fillColor = "transparent",
                  dashArray = "5, 5") %>% # Add polygons for Santa Monica Beach
      
      
      addPolygons(data = channel_islands, color = "white",weight = 3,smoothFactor = 1,
                  opacity = 1, fillOpacity = 0.25,fillColor = "transparent",
                  dashArray = "5, 5") %>%
      
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;

          // Create a custom control button
          var resetButton = L.control({position: 'topright'});
          
          resetButton.onAdd = function(map) {
            var div = L.DomUtil.create('div', 'leaflet-bar leaflet-control leaflet-control-custom');
            div.innerHTML = '<button type=\"button\">Reset Zoom</button>';
            div.style.width = '90px';
            div.style.height = '30px';
            div.style.lineHeight = '30px';
            div.style.textAlign = 'center';
            div.style.cursor = 'pointer';
            div.onclick = function(){
              map.setView([33.726973, -118.377620], 8.5);
            };
            return div;
          };
          
          resetButton.addTo(map);
        }
      ") %>% 
      
      # addLegend(values = NULL,
      #           title = '<small>Areas of Interest</small>',
      #           position = 'topright',
      #           colors = c("darkorange", "white", "red"),
      #           labels = c("Palos Verdes Shelf", "Study Area", "Barrel field of DDT-laced sludge")) %>% # Add legend
      # 
      setView(lng = -118.377620, lat = 33.726973, zoom = 9) %>% # Set initial view
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>% # Add mini map
      addMarkers(lat = 33.726973,lng = -118.377620,
                 options = markerOptions(draggable = TRUE)) %>% # Add draggable marker
      addCircleMarkers(lng = -118.48, 
                       lat = 33.55, 
                       color = "red",
                       radius = 20,
                       popup = "Barrel field of DDT-laced sludge")
                       # fill = TRUE, 
                       # fillColor = "red",
                       # fillOpacity = 0.7,
                       # stroke = FALSE,
                       # options = pathOptions(pane = "fixed"))# Add circle marker for barrel field
  
                       })
  
  ### Observe Marker Drag Event ###--------------
  
  observeEvent(input$locationMap_marker_dragend, {
    # Update current_markers latitude and longitude when marker is dragged
    current_markers$lat <- input$locationMap_marker_dragend$lat
    current_markers$long <- input$locationMap_marker_dragend$lng
  })  
  
  ### Update Selectize Input Choices ###---------
  
  updateSelectizeInput(session, 'CompositeCommonName', choices = species_name_clean, server = TRUE) # Update selectize input choices for fish species
  
  
  ### Observe Predict Button Click ###-----------
  
  observeEvent(input$predict_button, {
    # Retrieve user input and calculate prediction
    species <- input$CompositeCommonName # Get the selected fish species
    latitude <- current_markers$lat  # Get the current latitude from reactiveValues
    longitude <- current_markers$long  # Get the current longitude from reactiveValues
    advisory_name <- get_advisory(lat = current_markers$lat, long = current_markers$long) # Get the advisory area name
    
    # Construct image paths
    path = "data/OEHHA/" # Path for advisory images
    species_name_advisory <- input$species # Get the species name for advisory
    species_name_img <- tolower(gsub(" ", "-", input$species)) # Format species name for image file
    image_path <- read_csv(paste0(path, advisory_name, "/other_advisory.csv")) %>% 
      filter(Species == species_name_advisory) # Read advisory image path from CSV
    
    path2 = "data/fish_image/" # Path for fish images
    species_img <- tolower(gsub(" ", "-", input$species)) # Format species name for image file
    image_path2 <- paste0(path2, species_img, ".png") # Construct fish image path
    
    # Calculate prediction and serving size
    prediction <- predict_DDT(species, latitude, longitude) # Call the prediction function
    assignment_of_serving <- data.frame(pred = prediction) %>% 
      mutate(rec = ifelse(prediction <= 21,
                          8,
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
                                                                                  0,
                                                                                  NA))))))))),
             label = ifelse(rec == 8,
                            "7+",
                            rec))
  
    
    serving_size <- as.character(assignment_of_serving[1, 2]) # Get the serving size recommendation
    
    # make the serving size bar
    # composite score v2
    assignment_df <- reactive(assignment_of_serving)
    # Function to create gradient data frame
    create_gradient_df <- function(n = 8) {
      data.frame(
        x = seq(0, 8, length.out = n),
        color = colorRampPalette(c("red", "green"))(n)
      )
    }
    
    
    output$servings <- renderPlot({
      gradient_df <- create_gradient_df()
      
      ggplot(assignment_df(), aes(y = as.factor(1))) +  # need to create a dummy y-axis
        
        geom_tile(data = gradient_df, aes(x = x, y = 1, fill = color), 
                  color = "black",
                  height = 0.75,
                  width = 1.14) +
        scale_fill_identity() +  # Use the fill color directly
        # plot a red line at the value of the hazard score
        geom_segment(aes(y = 0.5, yend = 1.5, x = rec, xend = rec),
                     color = "black",
                     linewidth = 1.5) +
        # label the hazard score
        geom_text(aes(x = rec, y = 1, label = label),
                  hjust = -.2, color = "black", size = 8) +
        xlim(0, 8) +
        labs(y = NULL,
             x = NULL,
             title = "Serving Size") +
        scale_x_continuous(
          breaks = c(0, 8),  # specify where to place the labels
          labels = c("Do Not Eat", "Safe")  # specify the labels
        ) +
        theme(plot.margi4n = unit(c(0, 0, 0, 0), "lines")) +
        theme_minimal() +
        theme(aspect.ratio = 1/10, # adjust aspect ratio to move the plot title and x-axis labels closer
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 14))
    })
    
    # Check if location is valid
    lonlat <- data.frame(cbind(long = longitude,lat = latitude)) # Create data frame with longitude and latitude
    lonlat_sf = st_as_sf(lonlat, coords=c("long", "lat"), crs="EPSG:4326") # Convert to spatial object
    polsf <- sf::st_as_sf(areas) # Convert areas data frame to spatial object
    nearest <- polsf[sf::st_nearest_feature(lonlat_sf, polsf) ,] # Find nearest polygon to the location
    meters <- st_distance(x = lonlat_sf, y = nearest) %>% 
      as.numeric() # Calculate distance to nearest polygon in meters
    
    ###------------The Output----------------###
    
    if (meters > 500) {
      # If location is invalid (distance > 500 meters), display an error message
      output$serving_size <- renderText({ NULL })
      output$prediction <- renderText({ NULL })
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
        # Display the recommended serving size using the value from 'serving_size'
        output$serving_size <- renderText({
          paste("Based on these results, the recommended serving size for this fish at this location is ",serving_size, " per week. For information about serving size click the info button above.")
        })
        
        # If prediction is available, render the predicted value in the format of ng/g lipid
        output$prediction <- renderText({ NULL })
        output$prediction <- renderText({
          paste("There are ", round(prediction, 2), "ng of DDT per gram of ", species_name_advisory, ".")
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
  
  ### Observe Info Message Show Event ###--------
  observeEvent(input$show_info_message, {
    showNotification( # Show a notification when the info message is clicked
      input$show_info_message,
      duration = 5000,
      type = "message"
    )
  })
  
}