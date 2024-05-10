# LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

#install.packages("shinyBS")
library(shinyBS)
library(leaflet)
#install.packages("leaflet.extras")
library(leaflet.extras)
library(shinycssloaders)
library(extrafont)
library(showtext)
library(markdown)
library(rfishbase)

library(skimr)
library(tidymodels)
library(caret)
# Load the corrplot package
#library(corrplot)
#library(sjPlot)
#library(rstanarm)

library(terra)

#install.packages("reactlog")
library(reactlog)
reactlog_enable()

# packages for lat and long 
library(geos)
library(tidyverse)
library(sf)
library(raster)
library(sp)
library(brms)


# the 3 OEHHA polygons for coastal health advisories
ventura <- read_sf("data/polygons/venturaharbor.kml") %>%
  st_zm() %>% 
  mutate(Name = "venturaharbor")
  
smbeach <- read_sf("data/polygons/smbeach_to_sb.kml") %>%
  st_zm() %>% 
  mutate(Name = "smbeach_to_sb")
  #st_as_sf()
sbpier <- read_sf("data/polygons/sbpiersanmateopoint.kml") %>%
  st_zm() %>% 
  mutate(Name = "sbpiersanmateopoint")


shelf <- read_sf("data/polygons/Palos_Shelf.kml") %>%
  st_zm() %>%
  mutate(Name = "palosshelf")

# make dataframe of advisories
advisory_areas <- rbind(ventura, smbeach, sbpier) %>% 
  dplyr::select(Name, geometry)

# mission <- read_sf("shinydashboard/data/polygons/mission_bay.kml") %>%
#   st_zm()
 
# sd_bay <- read_sf("shinydashboard/data/polygons/san_diego_bay.kml") %>%
#   st_zm()


#sdbay <- read_sf("shinydashboard/data/polygons/san_diego_bay.kml")

# READ IN DATA ----

fish_data <- read_csv("data/fish_data_preprocessed.csv")
#view(fish_data)

fish_clean <- read_csv("data/fish_clean.csv") 


fish_data_clean <- na.omit(fish_data)  # Assuming columns 2 and 3 contain the coordinates

# Convert dataframe to sf object
#fish_coord <- st_as_sf(fish_data_clean, coords = c(1, 2))  

fish_coord <- st_as_sf(fish_data_clean, coords = c(2,3))

areas <- readRDS("data/pelagic_nearshore_fish_zones.rds") %>% 
  left_join(fish_data, join_by(Name == CompositeStationArea))


# data and data wrangling for model 

fish_clean_names <- as.data.frame(unique(fish_clean$scientific_name)) %>% 
  mutate(species = `unique(fish_clean$scientific_name)`) %>% 
  dplyr::select('species')


# get the unique species names - should be 61
species_name <- unique(fish_clean$scientific_name)

# Recode them into the names that are included in fishbase 

# update the names of the fish to match fishbase
species_name_clean <- as.data.frame(species_name) %>% 
  dplyr::mutate(species_name = case_when(species_name == "Embiotica jacksoni" ~ "Embiotoca jacksoni",
                                         species_name ==  "Rhinobatos productus" ~ "Pseudobatos productus",
                                         TRUE ~ species_name))

# load fish taxa
taxa <- rfishbase::load_taxa()

# filter the taxa based on name 
taxa_filter <- taxa %>% 
  filter(Species %in% species_name_clean$species_name) %>% 
  dplyr::select(scientific_name = Species, Family, Genus)

fish.clean.fam <- fish_clean %>% 
  dplyr::mutate(scientific_name = case_when(scientific_name == "Embiotica jacksoni" ~ "Embiotoca jacksoni",
                                            scientific_name ==  "Rhinobatos productus" ~ "Pseudobatos productus",
                                            TRUE ~ scientific_name)) %>%
  left_join(taxa_filter, by = "scientific_name") %>% 
  dplyr::mutate(Family = ifelse(scientific_name == "Doryteuthis opalescens",
                                "Loliginidae",
                                Family))

# load in dataframe with cleaned species name, and life history characterstics
fish_lh <- read_csv("data/species_common_science.csv") 


set.seed(123)

# Loading Bayesian regression model for prediction
brm.diet.habitat.year.fam.clean = readRDS("data/brm_mod.rda")
#brm.diet.habitat.year.fam.clean = readRDS(here::here("shinydashboard", "data", "speciesRandEffect.rda"))


## If application is in testing mode, will run all tests.
if (Sys.getenv("SHINY_TEST") == "true") {
  testthat::test_dir("tests/testthat")
}

