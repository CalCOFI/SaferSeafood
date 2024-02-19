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
  menuItem(text = "Dashboard", tabName = "dashboard", icon = icon("gauge"))
  
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
          column(width = 6,
                
                 #background info box ----
                 box(width = NULL,
                     title = tagList(icon("water"), strong("Background")),
                     "The harmful effects of Dichlorodiphenyltrichloroethane (DDT), and its breakdown products (DDX), have triggered widespread concerns, particularly due to the recent rediscovery of a barrel field containing DDT-laced sludge off the Southern California coast. This alarming find has not only captured the public's attention but has also highlighted its potential threats to human and environmental health. The negative side effects of DDT, including heightened cancer risks, premature births, developmental abnormalities, and neurological diseases in both humans and animals, have raised concerns of consuming seafood from the contaminated area. The consequences go beyond immediate health worries, also affecting the local economy and the well-being of recreational fishing communities.",
                     tags$img(src = "figure.png", 
                              alt = "Map of fishing zones and the number of fish samples through time, by region (inset). Nearshore 708 polygons are derived from McLaughlin et al. (2021) and pink blocks are California Department of Fish and Game 256 km2 709 fishing blocks.",
                              style = "max-width: 70%; display: block; margin: 0 auto;"),
                     tags$h6(tags$em("Map Source:", tags$a(href = "link", "link")),
                             style = "text-align: center;")
                     
                 ) # END background info box 
                
            ), # END left-hand column 
          
          #right - hand column ----
            column(width = 6,
                   
                   #first fluidRow ----
                   
                   fluidRow(
                     
                     # data source box ----
                    box(width = NULL,
                        title = tagList(icon("database"), strong("About The Data")),
                        "All resources employed in this study are provided by the client, Scripps Institute of Oceanography and California Cooperative Oceanic Fisheries Investigations (CalCOFI). Raster and tabular data collected across the Southern California Bight will be used to conduct this analysis."
                        
                    ) #END data source box  
                  
                   ), # END first fluidRow
                   
                   # second fluidRow ----
                   fluidRow(
                     
                     #disclaimer box ----
                     box(width = NULL,
                         title = tagList(icon("fish"), strong("About")),
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
                 
                 title = tags$strong("Adjust DDT ranges:"),
                 
                 # sliderInputs ----
                 sliderInput(inputId = "DDT_slider_input", label = "DDT Concentration:",
                             min = min(fish_data$AvgDDT), max = max(fish_data$AvgDDT),
                             value = c(min(fish_data$AvgDDT), max(fish_data$AvgDDT)))
                 
              ), # END input box ----
           
             # leaflet box ----
             box(width = 8,
                 
                 title = tags$strong("Fishing Zones:"),
                      
                  #leafleft output ----
                  leafletOutput(outputId = "fish_map_output") %>%
                    withSpinner(type = 1, color = "#4287f5")
                                  
                 
                 ) # END leaflet box
             
             ) # END fluidRow
           
           
    ), #END dashboard tabItem
  
  # advisory tabItem ----
  tabItem(tabName = "Seafood Advisories",
          
          # left - hand column ----
          column(width = 6,
                 
                 #other chemical advisory info box ----
                 box(width = NULL,
                     title = tagList(icon("bookmark"), strong("Mercury and PCB Consumption Advice")),
                     "la di da for running purposes",
                     tags$img(src = "advising.png", 
                              alt = "For more information regarding OEHHA fish advisory program, visit https://oehha.ca.gov/fish/advisories",
                              style = "max-width: 70%; display: block; margin: 0 auto;")
                     
                 ) # END chemical advisory info box 
                 
          ), #end left hand box
  
  
        #right - hand column ----
        column(width = 6,
         
         #first fluidRow ----
         
         fluidRow(
           
           # data source box ----
           box(width = NULL,
               title = tagList(icon("plus"), strong("Series of Consumer Inputs")),
               "User Selection, here are the inputs"
               
           ) #END data source box  
           
         ), # END first fluidRow
         
         # second fluidRow ----
         fluidRow(
           
           #disclaimer box ----
           box(width = NULL,
               title = tagList(icon("circle-exclamation"), strong("Outputs")),
               "How DDT cocentrations will be voiced to the public"
               #style = "background-color: #ff0000;"  # change color as needed
               
           ) # END disclaimer box
           
         ) # END second fluidRow
         
    ) # END right - hand column
  

  ) # END tabItems

  ) # END dashboardBody 

)


#..................combine all in dashboardPage..................
dashboardPage(header, sidebar, body)


