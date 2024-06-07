# SaferSeafood Shiny


## Overview
This repository containing files and data for developing the R Shiny online application that will be used by Master of Environmental Data Science students from UCSB's Bren School of Environmental Science and Management to fulfill their capstone project with Scripps institute of Oceanography and the California Cooperative Oceanic Fisheries Investigation (CalCOFI). This dashboard implements an updated bayesian statistical model and allows anglers to input catch and location in order to receive predicted DDT concentration and consumption advisories. The final product is a user-friendly web application tool that uses species and location to receive catch-specific DDT concentration predictions, recommended number of servings, and additional information on California consumption advisories related to Mercury and PCB. This application will be hosted and maintained by CalCOFI on their organization’s independent infrastructure.


## About the Data

## About the Files

global.R: Helps reduce redundant code, increase the apps speed, better code organization 

ui.R: for the layout that sets up the basic isual structure of the page and scales components in real time to fill all available broswer width 

server.R: contains the instructions that the computer needs to build the app

www: stores the elements to be rendered in the web browser such as image files, HTML, CSS, and JavaScript files 

scratch: all scripts not used in the app (data wrangling, practice data visualizations) 

This research project was designed for educational purposes. The information provided here does not come from any public agency and we are not making health recommendations

## Structure 

> ```
> Shiny-Dashboard
> │   README.md
> │   SaferSeafood_Shiny_Dashboard.Rproj
> │  .gitignore
> │   session_info.txt
> |   output.txt
> └───raw_data
>       │ fish_clean.csv
> └───scratch
>       │ serving_size.Rmd
>       │ practice-script-shinydashboard.R
>       | practice-script.R
>       | nearest-script.R
>       | data-proccessing-shinydashboard.R
> └───shinydashboard
>       │ www
>       │ ui.R
>       | server.R
>       | global.R
>       | rsconnect
>       | data
>
