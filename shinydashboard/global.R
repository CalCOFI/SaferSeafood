# LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(shinycssloaders)
library(extrafont)
library(showtext)

# READ IN DATA ----

fish_data <- read_csv("data/fish_data_preprocessed.csv")


#covariates: weight, length, location, and species 
# use a button for calucalting DDT 

#need numeric inputboxes: weight, lenght, lat, long, species 