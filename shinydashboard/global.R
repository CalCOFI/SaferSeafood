# global.R

 ## LOAD LIBRARIES ----
 
# Suppressing package startup messages to keep the console clean
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(shinyWidgets)
  library(shinyBS)
  library(leaflet)
  library(leaflet.extras)
  library(shinycssloaders)
  library(extrafont)
  library(showtext)
  library(markdown)
  library(rfishbase)
  library(sf)
  library(skimr)
  library(tidymodels)
  library(caret)
  library(corrplot)
  library(sjPlot)
  library(rstanarm)
  library(terra)
  library(reactlog)
  library(geos)
  library(tidyverse)
  library(raster)
  library(sp)
  library(brms)
  # install.packages("shiny.i18n")
  # library(shiny.i18n)
})

# Enabling Reactlog for debugging
reactlog_enable()

# customIcon <- makeIcon(
#   iconUrl = "https://fontawesome.com/icons/fishing-rod?f=classic&s=thin",  # URL to the marker icon image
#   iconWidth = 38,  # Width of the icon image
#   iconHeight = 95,  # Height of the icon image
#   iconAnchorX = 22,  # X coordinate of the "tip" of the icon (relative to its top left corner)
#   iconAnchorY = 94   # Y coordinate of the "tip" of the icon (relative to its top left corner)
# )

 ## LOAD SPATIAL DATA ----s

# Loading spatial data for coastal health advisories and other geographic features

# Ventura harbor advisory polygons
ventura <- read_sf("data/polygons/venturaharbor.kml", layer = "VenturaHarbortoSMPier.kmz") %>%
  st_zm() %>% 
  mutate(Name = "venturaharbor") %>% 
  st_transform(crs = 4326)

# Santa Monica to Santa Barbara Pier advisory polygons
smbeach <- read_sf("data/polygons/smbeach_to_sb.kml", layer = "SMPiertoSBPier.kmz") %>%
  st_zm() %>% 
  mutate(Name = "smbeach_to_sb") %>% 
  st_transform(crs = 4326)

# Santa Barbara Pier to San Mateo advisory polygons
sbpier <- read_sf("data/polygons/sbpiersanmateopoint.kml", layer = "SBPiertoSanMateoPoint.kmz") %>%
  st_zm() %>% 
  mutate(Name = "sbpiersanmateopoint") %>% 
  st_transform(crs = 4326)

# Palos Verdes Shelf polygon
shelf <- read_sf("data/polygons/Palos_Shelf.kml", layer = "Palos Verdes Shelf") %>%
  st_zm() %>%
  mutate(Name = "palosshelf") %>% 
  st_transform(crs = 4326)

# Fishing piers in California polygons
piers <- read_sf("data/polygons/Piers.kml") %>%
  st_transform(crs = 4326)

# Channel islands polygons``
channel_islands <- sf::st_read("data/polygons/cinms_py2") %>%
  st_transform(crs = 4326)
channel_islands <- channel_islands[-1, , drop = FALSE]

# Combining advisory areas into a single dataframe
advisory_areas <- rbind(ventura, smbeach, sbpier) %>% 
  dplyr::select(Name, geometry)



 ## LOAD AND PROCESS DATA ----

# Reading in fish data and associated geographic zones

# Fish data
fish_data <- read_csv("data/fish_data_preprocessed.csv")
fish_clean <- read_csv("data/fish_clean.csv")

# Pelagic and nearshore fish zones
areas <- readRDS("data/pelagic_nearshore_fish_zones.rds") %>% 
  left_join(fish_data, by = c("Name" = "CompositeStationArea"))
fish_zones <- readRDS("data/pelagic_nearshore_fish_zones.rds")

# Converting fish zones to sf object
fish_zones_sf <- st_as_sf(fish_zones, wkt = "geometry", crs = 4326) %>%
  mutate(Name = ifelse(grepl("^[0-9]+$", Name), "na", Name))

# Ensuring valid sf object and geometry column
if (!inherits(fish_zones_sf, "sf")) {
  fish_zones_sf <- st_as_sf(fish_zones_sf)
}
if (is.null(st_geometry(fish_zones_sf))) {
  stop("No geometry column found in the data.")
}

# Function to remove overlapping areas from sf object
remove_overlaps <- function(sf_object) {
  non_overlapping <- st_make_valid(sf_object) %>%
    st_union() %>%
    st_collection_extract("POLYGON")
  return(non_overlapping)
}

# Applying function to remove overlaps
non_overlapping_zones <- remove_overlaps(fish_zones_sf)

# Creating sf object from non-overlapping zones
non_overlapping_sf <- st_sf(data = data.frame(id = seq(length(non_overlapping_zones))), 
                            geometry = non_overlapping_zones)

# fishIcon <- makeIcon(
#   iconUrl = "/Users/katebecker/Documents/Bren/Capstone/shiny-saferseafood/shinydashboard/www/fishIcon.png",
#   iconWidth = 38, iconHeight = 40,
#   iconAnchorX = 22, iconAnchorY = 20
# )
# 

 ## DATA CLEANING AND WRANGLING ----

# Cleaning and preparing data for analysis

# Removing NA values from fish data
fish_data_clean <- na.omit(fish_data)

# Converting fish data to sf object based on coordinates
fish_coord <- st_as_sf(fish_data_clean, coords = c(2,3))

# Creating dataframe with unique species names
fish_clean_names <- as.data.frame(unique(fish_clean$scientific_name)) %>% 
  mutate(species = `unique(fish_clean$scientific_name)`) %>% 
  dplyr::select('species')

# Getting unique species names (should be 61)
species_name <- unique(fish_clean$scientific_name)

# Recode species names to match those included in FishBase
species_name_clean <- as.data.frame(species_name) %>% 
  dplyr::mutate(species_name = case_when(
    species_name == "Embiotica jacksoni" ~ "Embiotoca jacksoni",
    species_name == "Rhinobatos productus" ~ "Pseudobatos productus",
    TRUE ~ species_name
  ))

# Loading fish taxa from FishBase and filtering based on species names
taxa <- rfishbase::load_taxa()
taxa_filter <- taxa %>% 
  filter(Species %in% species_name_clean$species_name) %>% 
  dplyr::select(scientific_name = Species, Family, Genus)

# Renaming fish species and joining with taxa data
fish.clean.fam <- fish_clean %>% 
  dplyr::mutate(scientific_name = case_when(
    scientific_name == "Embiotica jacksoni" ~ "Embiotoca jacksoni",
    scientific_name == "Rhinobatos productus" ~ "Pseudobatos productus",
    TRUE ~ scientific_name
  )) %>%
  left_join(taxa_filter, by = "scientific_name") %>% 
  dplyr::mutate(Family = ifelse(scientific_name == "Doryteuthis opalescens",
                                "Loliginidae",
                                Family))

# Loading dataframe with cleaned species names and life history characteristics
fish_lh <- read_csv("data/species_common_science.csv")

# Loading Bayesian regression model for prediction
# brm.diet.habitat.year.fam.clean <- readRDS("data/brm_mod.rda")

brm_mod <- readRDS("data/brm_species_model.rda")

# Disconnecting from database (assuming dbDisconnect() function exists)
db_disconnect()
