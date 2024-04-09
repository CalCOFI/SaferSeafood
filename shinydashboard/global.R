# LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(shinycssloaders)
library(extrafont)
library(showtext)
library(markdown)

library(rfishbase)


library(skimr)
library(tidymodels)
library(caret)
# Load the corrplot package
library(corrplot)
library(sjPlot)
library(rstanarm)

library(sf)
library(terra)
library(leaflet)

if("pacman" %in% installed.packages() == FALSE){install.packages("pacman")}
pacman::p_load(geojsonR, factoextra,sf,dplyr, ggplot2, maps, fields,raster,
               MuMIn, lubridate, tidyr,ggh4x, lme4,sdmTMB,inlabru,cowplot,marmap,sjPlot, tidyverse, plyr, tidybayes, brms, bayesplot, loo,ggeffects,
               DHARMa)



# READ IN DATA ----

fish_data <- read_csv("data/fish_data_preprocessed.csv")
#view(fish_data)

fish_clean <- read_csv("data/fish_clean.csv")



#covariates: weight, length, location, and species 
# use a button for calucalting DDT 

#need numeric inputboxes: weight, lenght, lat, long, species 


# data and data wrangling for model 

fish_clean_names <- as.data.frame(unique(fish_clean$scientific_name)) %>% 
  mutate(species = `unique(fish_clean$scientific_name)`) %>% 
  dplyr::select('species')


# get the unique species names - should be 61
species_name <- unique(fish_clean$scientific_name)

# look at the list of species using the names - these are different sizes
#species_info <- species(species_list = species_name)


# Comparing the number of fish in the fish_clean and the fish_lh
#fish_clean_names <- as.data.frame(unique(fish_clean$scientific_name)) %>% 
#mutate(species = `unique(fish_clean$scientific_name)`) %>% 
#dplyr::select('species')

#fish_lh_names <- as.data.frame(unique(species_info$species))


# Recode them into the names that are included in fishbase 

# update the names of the fish to match fishbase
species_name_clean <- as.data.frame(species_name) %>% 
  dplyr::mutate(species_name = case_when(species_name == "Embiotica jacksoni" ~ "Embiotoca jacksoni",
                                         species_name ==  "Rhinobatos productus" ~ "Pseudobatos productus",
                                         TRUE ~ species_name))

# look at the taxa of the fish

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


