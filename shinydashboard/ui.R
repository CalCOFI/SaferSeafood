#........................dashboardHeader.........................
header <- dashboardHeader(
  title = tags$div(
    style = "position: relative; width: 100%;",
    tags$img(src = "scripps-logo.png", 
             style = "position: absolute; top: 0px; right: -20px; width: 250px; height: 50px;"),
    tags$img(src = "calcofi-logo.png", 
             style = "position: absolute; top: 0px; right: 235px; width: 50px; height: 50px;"),
    "SeaferSeafood"),
  titleWidth = 1200
  
) # END dashboardHeader

#........................dashboardSidebar........................

# add a how to use box to homepage 


sidebar <- dashboardSidebar(
  
  #sidebarMenu ----
  sidebarMenu(
    
    menuItem(text = "About", tabName = "about", icon = icon("star"), # previously welcome
             menuSubItem(text = "Project", tabName = "project", icon = icon("star")),
             menuSubItem(text = "User Manual", tabName = "user_manual", icon = icon("circle-exclamation")),
             menuSubItem(text = "Authors", tabName = "authors", icon = icon("gauge")),
             menuSubItem(text = "Data", tabName = "data", icon = icon("gauge"))),
    menuItem(text = "Whats In My Catch?", tabName = "whats_in_my_catch", icon = icon("gauge")), # previously dashboard
    menuItem(text = "Resources", tabName = "resources", icon = icon("circle-exclamation"), # previously seafood_advisory
             menuSubItem(text = "Fish Identification", tabName = "fish_id", icon = icon("circle-exclamation")),
             menuSubItem(text = "Research", tabName = "research", icon = icon("circle-exclamation"))) 
    
  ) # END sidebarMenu
) # end dashboard Sidebar

#..........................dashboardBody.........................
body <- dashboardBody(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  # about tabItem ----
  tabItems(
    
    # about tabItem ----
    tabItem(tabName = "project",
            
            # left - hand column ----
            column(width = 12,
                   
                   #background info box ----
                   box(width = NULL,
                       title = tagList(strong("Project Background")),
                       "The harmful effects of Dichlorodiphenyltrichloroethane (DDT), and its breakdown products (DDX), have triggered widespread concerns, particularly due to the recent rediscovery of a barrel field containing DDT-laced sludge off the Southern California coast. This alarming find has not only captured the public's attention but has also highlighted its potential threats to human and environmental health. The negative side effects of DDT, including heightened cancer risks, premature births, developmental abnormalities, and neurological diseases in both humans and animals, have raised concerns of consuming seafood from the contaminated area. The consequences go beyond immediate health worries, also affecting the local economy and the well-being of recreational fishing communities.",
                       tags$img(src = "dumpsite.png.jpeg", 
                                alt = "Map of fishing zones and the number of fish samples through time, by region (inset). Nearshore 708 polygons are derived from McLaughlin et al. (2021) and pink blocks are California Department of Fish and Game 256 km2 709 fishing blocks.",
                                style = "max-width: 90%; display: block; margin: 0 auto;")
                       
                   ) # END background info box 
                   
            ), # END left-hand column 
            
    ), # END about tabItem
    
    # data tabItem ----
    tabItem(tabName = "data",
            
            #right - hand column ----
            column(width = 8,
                   
                   #first fluidRow ----
                   
                   fluidRow(
                     
                     # data source box ----
                     
                     box(width = NULL,
                         title = tagList(icon("database"), strong("The Data")),
                         "All resources employed in this study are provided by the client, Scripps Institute of Oceanography and California Cooperative Oceanic Fisheries Investigations (CalCOFI). Raster and tabular data collected across the Southern California Bight will be used to conduct this analysis."
                         
                     ) #END data source box  
                   ),
                   
                   fluidRow(
                     
                     box(width = NULL,
                         
                         title = tagList(icon("table"), strong("Citation")),
                         "All of the data used for this project has been collected from public data files, and all code and future data/modeling will be available publicly through the team's GitHub organization and repositories. All statistical and web application coding will be conducted in R within RStudio, so any interested parties will be able to reproduce any work in R. ",
                         
                         
                     ) #END data source box  
                     
                   ), # END first fluidRow
                   
                   # second fluidRow ----
                   fluidRow(
                     
                     #disclaimer box ----
                     box(width = NULL,
                         
                         title = tagList(icon("triangle-exclamation"), strong("Disclaimer")),
                         "There are no restrictions on sharing the data and any outputs that result from this project, but all sources of data should be referenced.",
                         
                         #style = "background-color: #ff0000;"  # change color as needed
                         
                         
                     ) # END disclaimer box
                     
                   ) # END second fluidRow
                   
            ) # END right - hand column
            
            
    ), #END data tabItem
    
    # fish_identification tabItem ----
    tabItem(tabName = "fish_identification",
            
            # fluidRow ----
            
            fluidRow(
              
              # input box ----
              box(width = 4,
                  
                  title = tags$strong("Adjust DDT Range:"),
                  
                  # sliderInputs ----
                  sliderInput(inputId = "DDT_slider_input", label = "DDT (ng/g):",
                              min = min(fish_data$AvgDDT), max = max(fish_data$AvgDDT),
                              value = c(min(fish_data$AvgDDT), max(fish_data$AvgDDT)))
                  
              ), # END input box ----
              
              # leaflet box ----
              box(width = 8,
                  
                  title = tags$strong("Fishing Zones:"),
                  
                  #leafleft output ----
                  leafletOutput(outputId = "fish_map_output") %>%
                    withSpinner(type = 1, color = "#4287f5")
                  
                  
              ), # END leaflet box
              
              box(width = 6,
                  
                  title = tagList(strong("California Department of Fish and Wildlife")),
                  "",
                  tags$img(src = "fishing.png", 
                           alt = "For more information regarding OEHHA fish advisory program, visit https://oehha.ca.gov/fish/advisories.",
                           style = "max-width: 80%; display: block; margin: 0 auto;")
                  
                  
              ) # END fishing zone map box 
              
              
            ) # END fluidRow
            
            
    ), #END fish_identification tabItem
    
    # whats_in_my_catch tabItem
    tabItem(tabName = "whats_in_my_catch",
            
            # Left-hand column content
            column(width = 6,
                   
                   #other chemical advisory info box
                   box(width = NULL,
                       title = tagList(strong("Mercury and PCB Consumption Advice")),
                       "",
                       tags$img(src = "fish.png", 
                                alt = "For more information regarding OEHHA fish advisory program, visit https://oehha.ca.gov/fish/advisories.",
                                style = "max-width: 90%; display: block; margin: 0 auto;")
                   ), # END chemical advisory info box 
                   
                   box(width = NULL,
                       title = tagList(strong("Following Guidelines")),
                       "",
                       tags$img(src = "advising-fish-guide.png",
                                alt = "Guidelines",
                                style = "max-width: 90%; display: block; margin: 0 auto;")
                   ) # end GUIDELINES box 
                   
            ), #end left hand box
            
            #right - hand column
            column(width = 6,
                   fluidRow(
                     box(width = NULL,
                         title = tagList(icon("plus"), strong("Series of Consumer Inputs")), 
                         status = "primary", collapsible = TRUE,
                         "Caught a fish off the coast of Southern California? Fill the required fields below to better understand the levels of contamination.",
                         textInput("CompositeCommonName", "Species:"),
                         numericInput("CompositeTargetLatitude", "Latitude:", value = NULL),
                         numericInput("CompositeTargetLongitude", "Longitude:", value = NULL),
                         actionButton("predict_button", "Predict")
                     ), 
                     
                     #START Prediction Box
                     box(width = NULL, title = "Prediction Result", status = "success", solidHeader = TRUE,
                         collapsible = TRUE,
                         verbatimTextOutput("prediction")
                     )  #END Prediction Box
                     
                   ) # END first fluidRow
                   
            ) # END right-hand column
            
    ) # end whats_in_my_catch tabItem
    
    
  ) # END tabItems
  
) # END dashboardBody 



#..................combine all in dashboardPage..................

dashboardPage(header, sidebar, body, skin = "black")






