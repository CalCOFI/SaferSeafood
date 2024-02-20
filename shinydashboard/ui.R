#........................dashboardHeader.........................
header <- dashboardHeader(
  
  #title ----
  title = "Improving Access to Fish Consumption Advisories and Maintaining Confidence in California's Healthy Seafood Products",
  titleWidth = 1200
  
) # END dashboardHeader

#........................dashboardSidebar........................
sidebar <- dashboardSidebar(
  
  #sidebarMenu ----
  sidebarMenu(
  
  menuItem(text = "Welcome", tabName = "welcome", icon = icon("star")),
  menuItem(text = "Dashboard", tabName = "dashboard", icon = icon("gauge")),
  menuItem(text = "Seafood Advisory", tabName = "seafood_advisory", icon = icon("circle-exclamation"))
  
  ) # END sidebarMenu 
) # end dashboard Sidebar

#..........................dashboardBody.........................
body <- dashboardBody(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  # welcome tabItem ----
  tabItems(
    
    # welcome tabItem ----
    tabItem(tabName = "welcome",
          
          # left - hand column ----
          column(width = 8,
                
                 #background info box ----
                 box(width = NULL,
                     title = tagList(icon("water"), strong("Background")),
                     "The harmful effects of Dichlorodiphenyltrichloroethane (DDT), and its breakdown products (DDX), have triggered widespread concerns, particularly due to the recent rediscovery of a barrel field containing DDT-laced sludge off the Southern California coast. This alarming find has not only captured the public's attention but has also highlighted its potential threats to human and environmental health. The negative side effects of DDT, including heightened cancer risks, premature births, developmental abnormalities, and neurological diseases in both humans and animals, have raised concerns of consuming seafood from the contaminated area. The consequences go beyond immediate health worries, also affecting the local economy and the well-being of recreational fishing communities.",
                     tags$img(src = "dumpsite.png.jpeg", 
                              alt = "Map of fishing zones and the number of fish samples through time, by region (inset). Nearshore 708 polygons are derived from McLaughlin et al. (2021) and pink blocks are California Department of Fish and Game 256 km2 709 fishing blocks.",
                              style = "max-width: 90%; display: block; margin: 0 auto;")
                     
                 ) # END background info box 
                
            ), # END left-hand column 
          
          #right - hand column ----
            column(width = 4,
                   
                   #first fluidRow ----
                   
                   fluidRow(
                     
                     # data source box ----
                    box(width = NULL,
                        title = tagList(icon("database"), strong("The Data")),
                        "All resources employed in this study are provided by the client, Scripps Institute of Oceanography and California Cooperative Oceanic Fisheries Investigations (CalCOFI). Raster and tabular data collected across the Southern California Bight will be used to conduct this analysis."
                        
                    ) #END data source box  
                  
                   ), # END first fluidRow
                   
                   # second fluidRow ----
                   fluidRow(
                     
                     #disclaimer box ----
                     box(width = NULL,
                         title = tagList(icon("fish"), strong("The Dashboard")),
                         "This tool will allow users to input specific details such as species, location, and demographic information, yielding personalized and precise predictions of DDT concentrations along with corresponding advisories."
                         #style = "background-color: #ff0000;"  # change color as needed
                         
                         ) # END disclaimer box
                     
                   ) # END second fluidRow
                   
            ) # END right - hand column
          
  ), # END welcome tabItem
  
  # dashboard tabItem ----
  tabItem(tabName = "dashboard",
          
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
          
           
    ), #END dashboard tabItem
  
  # advisory tabItem ----
  tabItem(tabName = "seafood_advisory",
          
          # left - hand column ----
          column(width = 6,
                 
                 #other chemical advisory info box ----
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
  
  
        #right - hand column ----
        column(width = 6,
         
         #first fluidRow ----
         
         fluidRow(
           
           # data source box ----
           box(width = NULL,
               title = tagList(icon("plus"), strong("Series of Consumer Inputs")),
               "Caught a fish off the coast of Southern California? Fill the required fields below to better understand the levels of contamination."
               
           ) #END data source box  
           
         ), # END first fluidRow
         
        
         # adding text box inputs
         fluidPage(
           
           textInput("Species", "What species did you catch?"),
           textInput("Weight", "Weight of Species"),
           textInput("Length", "Length of Species"),
           textInput("Latitude", "What's your latitude?"),
           textInput("Longitude", "What's yours Longitude?")
         ), #end fluid page 
         
         
         # second fluidRow ----
         fluidRow(
           
           #disclaimer box ----
           box(width = NULL,
               title = tagList(icon("circle-exclamation"), strong("Outputs")),
               "Click the button below to output the DDT concentration for your catch!"
               #style = "background-color: #ff0000;"  # change color as needed
               
           ) # END disclaimer box
           
         ), # end second fluid row ----
         
         fluidPage(
           actionButton("click", "DDT Concentraiton Advisory")
           
         ) #end fluid page 
           
             
         
    ) # END right - hand column
    
  ) # end seafood advisory tab 

  ) # END tabItems

) # END dashboardBody 




#..................combine all in dashboardPage..................
dashboardPage(header, sidebar, body)


