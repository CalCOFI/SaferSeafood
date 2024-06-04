# SaferSeafood Shiny


## Overview
This repository containing files and data for developing the R Shiny online application that will be used by Master of Environmental Data Science students from UCSB's Bren School of Environmental Science and Management to fulfill their capstone project with Scripps institute of Oceanography and the California Cooperative Oceanic Fisheries Investigation (CalCOFI). This dashboard implements an updated bayesian statistical model and allows anglers to input catch and location in order to receive predicted DDT concentration and consumption advisories. The final product is a user-friendly web application tool designed to assist fishermen and environmental researchers by predicting DDT concentrations in various fish species based on their geographic catch location and catch species.

## About the Data

## About the Files

## Structure 

├── README.md
├── SaferSeafood_Shiny_Dashboard.Rproj
├── old_server.R
├── output.txt
├── raw_data
│   └── fish_clean.csv
├── scratch
│   ├── data-processing-shinydashboard.R
│   ├── nearest-scrpt.R
│   ├── practice-script-app1.R
│   ├── practice-script-shinydashboard.R
│   └── serving_size.Rmd
├── session_info.txt
└── shinydashboard
    ├── data
    │   ├── OEHHA
    │   │   ├── sbpiersanmateopoint
    │   │   │   └── other_advisory.csv
    │   │   ├── smbeach_to_sb
    │   │   │   └── other_advisory.csv
    │   │   └── venturaharbor
    │   │       └── other_advisory.csv
    │   ├── brm_mod.rda
    │   ├── brm_species_model.rda
    │   ├── fish_clean.csv
    │   ├── fish_data_preprocessed.csv
    │   ├── fish_image
    │   │   ├── barred-sand-bass.png
    │   │   ├── barred-surfperch.png
    │   │   ├── black-croaker.png
    │   │   ├── black-perch.png
    │   │   ├── blue-rockfish.png
    │   │   ├── brown-rockfish.png
    │   │   ├── brown-smoothhound-shark.png
    │   │   ├── california-corbina.png
    │   │   ├── california-halibut.png
    │   │   ├── california-lizardfish.png
    │   │   ├── california-scorpionfish.png
    │   │   ├── california-sheephead.png
    │   │   ├── canary-rockfish.png
    │   │   ├── chillipepper-rockfish.png
    │   │   ├── chub-mackerel.png
    │   │   ├── copper-rockfish.png
    │   │   ├── diamond-turbot.png
    │   │   ├── fantail-sole.png
    │   │   ├── flag-rockfish.png
    │   │   ├── gopher-rockfish.png
    │   │   ├── gray-smoothhound-shark.png
    │   │   ├── greenblotched-rockfish.png
    │   │   ├── greenspotted-rockfish.png
    │   │   ├── halfmoon.png
    │   │   ├── hornyhead-turbot.png
    │   │   ├── jacksmelt.png
    │   │   ├── kelp-bass.png
    │   │   ├── kelp-rockfish.png
    │   │   ├── leopard-shark.png
    │   │   ├── longfin-sanddab.png
    │   │   ├── market-squid.png
    │   │   ├── northern-anchovy.png
    │   │   ├── ocean-whitefish.png
    │   │   ├── olive-rockfish.png
    │   │   ├── opaleye.png
    │   │   ├── pacific-barracuda.png
    │   │   ├── pacific-sardine.png
    │   │   ├── pile-surfperch.png
    │   │   ├── queenfish.png
    │   │   ├── quillback-rockfish.png
    │   │   ├── rainbow-surfperch.png
    │   │   ├── rosy-rockfish.png
    │   │   ├── shiner-surfperch.png
    │   │   ├── shovelnose-guitarfish.png
    │   │   ├── slough-anchovy.png
    │   │   ├── speckled-rockfish.png
    │   │   ├── speckled-sandbab.jpeg
    │   │   ├── spiny-dogfish.png
    │   │   ├── spotfin-croaker.png
    │   │   ├── spotted-sand-bass.png
    │   │   ├── spotted-turbot.png
    │   │   ├── squarespot-rockfish.png
    │   │   ├── starry-rockfish.png
    │   │   ├── striped-mullet.png
    │   │   ├── topsmelt.png
    │   │   ├── vermillion-rockfish.png
    │   │   ├── walleye-surfperch.png
    │   │   ├── white-croaker.png
    │   │   ├── white-surfperch.png
    │   │   ├── yellowfin-croaker.png
    │   │   └── yellowtail-rockfish.png
    │   ├── pelagic_nearshore_fish_zones.rds
    │   ├── polygons
    │   │   ├── Palos_Shelf.kml
    │   │   ├── Piers.kml
    │   │   ├── cinms_py2
    │   │   │   ├── WebMaps_CINMS.jpg
    │   │   │   ├── cinms_py.dbf
    │   │   │   ├── cinms_py.html
    │   │   │   ├── cinms_py.kml
    │   │   │   ├── cinms_py.prj
    │   │   │   ├── cinms_py.sbn
    │   │   │   ├── cinms_py.sbx
    │   │   │   ├── cinms_py.shp
    │   │   │   ├── cinms_py.shp.xml
    │   │   │   └── cinms_py.shx
    │   │   ├── sbpiersanmateopoint.kml
    │   │   ├── smbeach_to_sb.kml
    │   │   └── venturaharbor.kml
    │   ├── sediment_data
    │   │   ├── sediment_rasters
    │   │   │   ├── sediment_totalDDT_2003.grd
    │   │   │   ├── sediment_totalDDT_2003.gri
    │   │   │   ├── sediment_totalDDT_2008.grd
    │   │   │   ├── sediment_totalDDT_2008.gri
    │   │   │   ├── sediment_totalDDT_2013.grd
    │   │   │   ├── sediment_totalDDT_2013.gri
    │   │   │   ├── sediment_totalDDT_2018.grd
    │   │   │   └── sediment_totalDDT_2018.gri
    │   │   └── totalDDX_sediment_zone_summary.rds
    │   ├── species
    │   ├── speciesRandEffect.rda
    │   └── species_common_science.csv
    ├── global.R
    ├── rsconnect
    │   └── shinyapps.io
    │       └── saferseafood
    │           └── shinydashboard.dcf
    ├── server.R
    ├── ui.R
    └── www
        ├── CALCOFI-fish.png
        ├── OEHHA-serv.png
        ├── advising-fish-guide.png
        ├── advising.png
        ├── advisoryhelp.png.html
        ├── bren-white-logo.png
        ├── bren.png
        ├── calcofi-logo.png
        ├── dumpsite.png.jpeg
        ├── figure.png
        ├── fish-hand.png
        ├── fish-icon-mark.jpeg
        ├── fish-id.png
        ├── fish-serving-box.png
        ├── fish-serving.png
        ├── fish.png
        ├── fishIcon.png
        ├── fishicon.jpeg
        ├── fishing.png
        ├── poster-OEHHA.pdf
        ├── scripps-logo.png
        ├── scripps-ucsd-logo.png
        ├── styles.css
        └── white-scripps-logo.png
