
#..............................SETUP.............................

# load packages ----
library(tidyverse)
library(leaflet)

# read in data ----
fish_data <- read_csv(here::here("shinydashboard", "data", "fish_data_preprocessed.csv"))

#..........................PRACTICE VIZ..........................

leaflet() %>%
  
  # add titles
  addProviderTiles(providers$Esri.WorldImagery) %>%
  
  # set view over CA
  setView(lng = -117.784, lat = 30.0906, zoom = 6) %>%
  
  # add minimi map 
  addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
  
  # add markers
  addMarkers(data = fish_data,
             lng = fish_data$CompositeTargetLongitude, lat = fish_data$CompositeTargetLatitude,
             popup = paste0("Site Name: ", fish_data$CompositeStationArea, "<br>",
                            "Avg DDT: ", fish_data$AvgDDT, "<br>")
             )
  