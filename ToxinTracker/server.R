# Server function
server <- function(input, output, session) {
  
  ### Reactive Values & Data Preparation ###--------------
  
  filtered_fish_data <- reactive(fish_data) # Reactive expression for filtering fish data
  current_markers <- reactiveValues(lat = 33.726973, long = -118.377620, zoom = 10) # Reactive values for current marker position and zoom level
  
  ### Utility Functions ###------------
  
  # Advisory function 
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
  model <- brm_mod # Load the Bayesian regression model
  
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
    return(24)  # Placeholder constant value
  }
  
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
      CompositeCommonName = input_species$CompositeCommonName # Family of the species
    )
    
    # Check if all necessary data is available
    if (anyNA(new_data)) {
      # Change depending on what we want to output
      return(NA)
    }
    
    prediction <- as.data.frame(fitted(model, newdata = new_data, re.form = NA)) # Predict using the Bayesian model
    estimate <- prediction[1]
    estimate_trans <- exp(estimate) - 1 # Transform the estimate
    
    if ((estimate_trans) < 0) {
      return(0)
    } else {
      return(estimate_trans)}
  }
  
  
  
  # Prediction function using the Bayesian model
  predict_DDT1 <- function(species, lat, long) {
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
      CompositeCommonName = input_species$CompositeCommonName # Family of the species
    )
    
    # Check if all necessary data is available
    if (anyNA(new_data)) {
      # Change depending on what we want to output
      return(NA)
    }
    
    prediction <- as.data.frame(fitted(model, newdata = new_data, re.form = NA)) # Predict using the Bayesian model
    estimate <- prediction[3]
    estimate_trans1 <- exp(estimate) - 1 # Transform the estimate
    
    if ((estimate_trans1) < 0) {
      return(0)
    } else {
      return(estimate_trans1)}
  }
  
  predict_DDT2 <- function(species, lat, long) {
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
      CompositeCommonName = input_species$CompositeCommonName # Family of the species
    )
    
    # Check if all necessary data is available
    if (anyNA(new_data)) {
      # Change depending on what we want to output
      return(NA)
    }
    
    prediction <- as.data.frame(fitted(model, newdata = new_data, re.form = NA)) # Predict using the Bayesian model
    estimate <- prediction[4]
    estimate_trans2 <- exp(estimate) - 1 # Transform the estimate
    
    if ((estimate_trans2) < 0) {
      return(0)
    } else {
      return(estimate_trans2)}
  }
  
  
  # Create map layer manually for pre-existing dump sites based on lat and long coordinates
  dumpsite_area <- data.frame(
    lat = c(33.6, 33.6, 33.5, 33.4, 33.3, 32.8, 32.7, 32.6, 32.5, 33.6, 33.3, 33, 32.7, 33.55),
    lng = c(-118.6,-118.5,-118.4,-118.1, -117.8, -117.6, -117.7, -117.3, -117.2, -119.5, -118.8, -118.9, -119.1, -118.48),
    label = c("#1", "#2", "#3", "#4", "#5", "#6", "#7", "#8", "#9", "#10", "#11", "#12", "#13", "Most recent")
  )
  
  
  ### Leaflet Map Rendering ###------------------
  
  output$locationMap <- renderLeaflet({
    # Initialize the Leaflet map
    leaflet() %>%
      # Add base map tiles from Esri NatGeo World Map
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      
      # Add polygons for non-overlapping zones with specified styles
      addPolygons(data = non_overlapping_sf, 
                  color = "blue", weight = 3, smoothFactor = 1, opacity = .8, fillOpacity = 0.2, fillColor = "orange", dashArray = "3, 8") %>%
      
      # Add polygon for Palos Verdes Shelf with a popup
      addPolygons(data = shelf, color = "darkblue", 
                  label = "Palos Verdes Superfund Site",
                  popupOptions = popupOptions(maxWidth = "100%", closeOnClick = TRUE)) %>%
      
      # Add circle marker for the most recent barrel field
      addCircleMarkers(lng = -118.48, 
                       lat = 33.55, 
                       color = "red",
                       label = "Barrel field of DDT-laced sludge") %>% 
      
      ## Uncommented code for advisory borders (if needed in future) ##
      
      # Add polygons for Ventura with advisories
      # addPolygons(data= ventura, color = "white", weight = 3, smoothFactor = 1,
      #             opacity = 1, fillOpacity = 0.25, fillColor = "transparent", dashArray = "5, 5") %>%
      
      # Add polygons for Santa Barbara Pier with advisories
      # addPolygons(data= sbpier, color = "white", weight = 3, smoothFactor = 1,
      #             opacity = 1, fillOpacity = 0.25, fillColor = "transparent", dashArray = "5, 5") %>%
      
    # Add polygons for Santa Monica Beach with advisories
    # addPolygons(data= smbeach, color = "white", weight = 3, smoothFactor = 1,
    #             opacity = 1, fillOpacity = 0.25, fillColor = "transparent", dashArray = "5, 5") %>%
    
    # Add polygons for Channel Islands
    # addPolygons(data = channel_islands, color = "white", weight = 3, smoothFactor = 1,
    #             opacity = 1, fillOpacity = 0.25, fillColor = "transparent", dashArray = "5, 5") %>%
    
    # Add circle markers for existing DDT dumpsites
    addCircleMarkers(data = dumpsite_area, 
                     ~lng, ~lat,
                     group = "DDT Dumpsites") %>%
      
      # Add circle markers for fishing piers with popup information
      addCircleMarkers(
        data = clip_coords,
        # lng = ~st_coordinates(clip_coords)[, 1],
        # lat = ~st_coordinates(clipped_sf)[, 2],

        #popup = ~paste("Name:", Name, "<br>", "Description:", Description),

        # popup = ~paste("Name:", Name, "<br>", "Description:", Description),

        radius = 7,
        color = "#5C4033",
        opacity = .7,
        group = "Piers"
      ) %>%
      
      # Add layer control for toggling visibility of DDT dumpsites and piers
      addLayersControl(
        overlayGroups = c("DDT Dumpsites", "Piers"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("DDT Dumpsites") %>%
      hideGroup("Piers") %>%
      
      # Add custom control button for resetting map zoom and view
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
            div.onclick = function() {
              map.setView([33.726973, -118.377620], 8);
            };
            return div;
          };

          resetButton.addTo(map);

// Add event listener for map click
map.on('click', function(e) {
  // Pass the clicked latitude and longitude back to Shiny
  Shiny.setInputValue('clicked_lat', e.latlng.lat);
  Shiny.setInputValue('clicked_lng', e.latlng.lng);

  // Remove existing markers
  map.eachLayer(function (layer) {
    if (layer instanceof L.Marker) {
      map.removeLayer(layer);
    }
  });

  // Define a custom icon with the desired color
  var customIcon = L.icon({
    iconUrl: 'https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png',
    iconSize: [25, 41], // size of the icon
    iconAnchor: [12, 41], // point of the icon which will correspond to marker's location
    popupAnchor: [1, -34], // point from which the popup should open relative to the iconAnchor
    shadowUrl: 'https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png',
    shadowSize: [41, 41] // size of the shadow
  });

  // Add a new marker at the clicked location with the custom icon
  var marker = L.marker(e.latlng, { icon: customIcon }).addTo(map);
});
        }
      ") %>%
      
      # Set initial map view to a specific latitude, longitude, and zoom level
      setView(lng = -118.377620, lat = 33.726973, zoom = 8) %>%
      
      # Add a mini map with toggle display
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) %>%
      
      # Add marker at a specific location
      addMarkers(lat = 33.726973,lng = -118.377620,
                 #options = markerOptions(draggable = TRUE),
                 icon = makeIcon(
                   iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", 
                   iconWidth = 25, iconHeight = 41, 
                   iconAnchorX = 12, iconAnchorY = 41
                 )) 
  })
  
  ### Observe Marker Click Event ###--------------
  
  ### For Dragging
  # observeEvent(input$locationMap_marker_dragend, {
  #   # Update current_markers latitude and longitude when marker is dragged
  #   current_markers$lat <- input$locationMap_marker_dragend$lat
  #   current_markers$long <- input$locationMap_marker_dragend$lng
  # })  
  
  
  ###For Clicking
  observeEvent(input$clicked_lat, {
    # Update current_markers latitude and longitude when marker is clicked
    current_markers$lat <- input$clicked_lat
    current_markers$long <- input$clicked_lng
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
    prediction1 <- predict_DDT1(species, latitude, longitude) # Call the prediction2 function
    prediction2 <- predict_DDT2(species, latitude, longitude) # Call the prediction3 function
    
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
    
    # Make the serving size bar
    # Composite score v2
    assignment_df <- reactive(assignment_of_serving)  # Reactive expression for assignment data frame
    
    # Function to create gradient data frame
    create_gradient_df <- function(n = 8) {
      data.frame(
        x = seq(0, 8, length.out = n),  # Generate sequence of x values
        color = colorRampPalette(c("red", "green"))(n)  # Create gradient from red to green
      )
    }
    
    output$servings <- renderPlot({
      gradient_df <- create_gradient_df()  # Create gradient data frame
      
      ggplot(assignment_df(), aes(y = as.factor(1))) +  # Create dummy y-axis
        
        # Add gradient tiles
        geom_tile(data = gradient_df, aes(x = x, y = 1, fill = color), 
                  color = "black",
                  height = 0.75,
                  width = 1.14) +
        scale_fill_identity() +  # Use the fill color directly
        
        # Plot a black line at the value of the hazard score
        geom_segment(aes(y = 0.5, yend = 1.5, x = rec, xend = rec),
                     color = "black",
                     linewidth = 1.5) +
        
        # Label the hazard score
        geom_text(aes(x = rec, y = 1, label = label),
                  hjust = -0.1,
                  color = "black", size = 8) +
        
        # Set x-axis limits
        xlim(0, 8) +
        
        # Add plot labels
        labs(y = NULL,
             x = NULL,
             title = "Number of Servings Per Week") +
        
        # Customize x-axis labels
        scale_x_continuous(
          breaks = c(0, 8),  # Specify where to place the labels
          labels = c("Unsafe", "Safe")  # Specify the labels
        ) +
        
        # Apply minimal theme and customize appearance
        theme_minimal() +
        theme(aspect.ratio = 1/10,  # Adjust aspect ratio to move the plot title and x-axis labels closer
              plot.title = element_text(face = "bold", size = 20),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 14))
    })
    
    
    
    # Make the serving size bar
    # Composite score v2
    assignment_df <- reactive(assignment_of_serving)  # Reactive expression for assignment data frame
    
    #plot for advisory
    create_gradient_df <- function(n = 8) {
      data.frame(
        x = seq(0, 8, length.out = n),  # Generate sequence of x values
        color = colorRampPalette(c("red", "green"))(n)  # Create gradient from red to green
      )
    }
    
    output$servings <- renderPlot({
      gradient_df <- create_gradient_df()  # Create gradient data frame
      
      ggplot(assignment_df(), aes(y = as.factor(1))) +  # Create dummy y-axis
        
        # Add gradient tiles
        geom_tile(data = gradient_df, aes(x = x, y = 1, fill = color), 
                  color = "black",
                  height = 0.75,
                  width = 1.14) +
        scale_fill_identity() +  # Use the fill color directly
        
        # Plot a black line at the value of the hazard score
        geom_segment(aes(y = 0.5, yend = 1.5, x = rec, xend = rec),
                     color = "black",
                     linewidth = 1.5) +
        
        # Label the hazard score
        geom_text(aes(x = rec, y = 1, label = label),
                  hjust = -0.1,
                  color = "black", size = 8) +
        
        # Set x-axis limits
        xlim(0, 8) +
        
        # Add plot labels
        labs(y = NULL,
             x = NULL,
             title = "Number of Servings Per Week") +
        
        # Customize x-axis labels
        scale_x_continuous(
          breaks = c(0, 8),  # Specify where to place the labels
          labels = c("Unsafe", "Safe")  # Specify the labels
        ) +
        
        # Apply minimal theme and customize appearance
        theme_minimal() +
        theme(aspect.ratio = 1/10,  # Adjust aspect ratio to move the plot title and x-axis labels closer
              plot.title = element_text(face = "bold", size = 20),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(face = "bold",size = 14))
    })
    
    
    # Check if location is valid
    lonlat <- data.frame(cbind(long = longitude,lat = latitude)) # Create data frame with longitude and latitude
    lonlat_sf = st_as_sf(lonlat, coords=c("long", "lat"), crs="EPSG:4326") # Convert to spatial object
    polsf <- sf::st_as_sf(areas) # Convert areas data frame to spatial object
    nearest <- polsf[sf::st_nearest_feature(lonlat_sf, polsf) ,] # Find nearest polygon to the location
    meters <- st_distance(x = lonlat_sf, y = nearest) %>% 
      as.numeric() # Calculate distance to nearest polygon in meters
    
    ###------------The Output----------------###
    
    if (meters > 0) {
      # If location is invalid (distance > 500 meters), display an error message
      output$serving_size <- renderText({ NULL })
      output$prediction <- renderText({ NULL })
      output$advisory <- renderText({ NULL })
      output$servings <- renderPlot({ NULL })
      output$fish_image <- renderImage({ NULL })
      output$fish_error <- renderText({ NULL })
      output$validation_result <- renderText({
        "We’re sorry, but we are not able to make predictions for that location. We can only make valid predictions for the area outlined on the map."
      })
      
    } else {
      
      # Clear the validation_result output if the location is valid
      output$validation_result <- renderText({NULL})
      # Check if the 'prediction' value is missing (NA)
      if (is.na(prediction)) {
        # Handle the case where prediction is NA by providing an informative message
        output$prediction <- renderText({ NULL })
        output$advisory <- renderText({ NULL })
        output$fish_image <- renderImage({ NULL })
        output$fish_error <- renderText({
          "Please select a fish species before pressing the Predict button."
        })
        # Clear any previous error messages
        output$serving_size <- renderText({ NULL })
      } else {
        # Display the recommended serving size using the value from 'serving_size'
        output$serving_size <- renderText({
          paste0("The recommended number of servings per week for ", species_name_advisory, " at this location is ",serving_size,".")
        })
        
        # If prediction is available, render the predicted value in the format of ng/g lipid
        output$prediction <- renderText({ NULL })
        output$fish_error <- renderText({ NULL })
        output$prediction <- renderText({
          paste0("The range of predicted DDT concentration is between ", round(prediction1, 2), " to ", round(prediction2, 2)," ng of per gram of fish.")
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
                paste0("The recommended number of servings per week for all age groups is ", image_path[[2]], ".")
              })
            } else {  # If serving sizes for both age groups are different
              output$advisory <- renderText({  # Render the advisory message
                paste("The recommended number of servings per week for women 18-49 years and children 1-17 years is ", image_path[[2]], ". The recommended number of servings per week for women 50 years and older and men 18 years and older is ", image_path[[3]], ".")
              })
            }
          } else {  # If only one serving size is found
            output$advisory <- renderText({  # Render the advisory message
              paste("The recommended number of servings per week for all age and gender groups is ", image_path[[2]], ".")
            })
          }
        } else {  # If no image paths are found
          # If species not found, display "no advisories found"
          output$advisory <- renderText({  # Render the advisory message
            HTML("No other advisories found for your species at your location.")
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