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
  sidebarMenu(menuItem(text = "What's In My Catch?", tabName = "whats_in_my_catch"), # previously dashboard
              menuItem(text = "User Manual", tabName = "user_manual"),
              menuItem(text = "About", tabName = "about"), # previously welcome
              menuItem(text = "Resources", tabName = "resources", # previously seafood_advisory
                       menuSubItem(text = "Fish Identification", tabName = "fish_id"),
                       menuSubItem(text = "Research", tabName = "research")) 
              
  ) # END sidebarMenu
) # end dashboard Sidebar

#..........................dashboardBody.........................
body <- dashboardBody(
  
  # shinyjs::useShinyjs(),
  # tags$script(src = "functions.js"),
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
            
            
            # allow for user to use current location
            tags$script('
                $(document).ready(function () {
                  navigator.geolocation.getCurrentPosition(onSuccess, onError);
                
                  function onError (err) {
                    Shiny.onInputChange("geolocation", false);
                  }
                
                  function onSuccess (position) {
                    setTimeout(function () {
                      var coords = position.coords;
                      console.log(coords.latitude + ", " + coords.longitude);
                      Shiny.onInputChange("geolocation", true);
                      Shiny.onInputChange("lat", coords.latitude);
                      Shiny.onInputChange("long", coords.longitude);
                    }, 1100)
                  }
                });
                ')),
  
  
  # about tabItem ----
  tabItems(
    
    tabItem(tabName = "whats_in_my_catch",
            "Caught a fish off the coast of Southern California? Fill the required fields below to better understand the levels of contamination.",
            fluidRow(width = NULL,
                     # Add map box with point dragger
                     box(width = NULL,
                         leafletOutput(outputId = "locationMap"),
                         htmlOutput(outputId = "text"),
                         absolutePanel(
                           top = 50, left = 70, 
                           draggable = TRUE, 
                           width = "20%",
                           tags$style(HTML(".selectize-control.single .selectize-input {
                                        font-size: 16px;
                                      }")),
                           selectizeInput(inputId = "species", 
                                          label = tags$span("Select species:", style = "font-size: 16px;"),
                                          choices = str_to_title(fish_lh$CompositeCommonName),
                                          options = list(
                                            placeholder = 'Please select a species',
                                            onInitialize = I('function() { this.setValue(""); }')
                                          )
                           ),
                           checkboxInput(inputId = "use_location", "Use your current location?"),
                           actionButton("predict_button", "Predict!", class = "btn-primary")
                         )
                     )
                     ),
            
            fluidRow(width = NULL,
                     
                     box(width = 8,
                         title = tagList("Prediction Result"),
                         status = "success", 
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         verbatimTextOutput("prediction"),
                         tags$div(
                           style = "display: flex; align-items: center; word-wrap: break-word;",
                           verbatimTextOutput("serving_size"),
                           actionButton("info_button", style = "margin-left: 5px;", icon("info-circle"))
                         ),
                         
                         # create tooltip so that if you hover over the serving size the serving size info comes up
                         bsTooltip(id = "info_button", 
                                   title = "A serving size is defined by the OEHHA as an 8oz skinless fillet.")
                     ),
                     box(width = 9, 
                         title = "Health Advisories", status = "success", solidHeader = TRUE,
                         collapsible = TRUE,
                         imageOutput(outputId = "advisory_image")
                     )
            ) # end map row
            
    ), # END what's in my catch tab item
    
    # about tabItem ----
    tabItem(tabName = "about",
            fluidRow(
              tabBox(
                title = NULL, width = 12,
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "about", height = "250px",
                tabPanel("Project Background",
                         
                         #title box ----
                         box(width = 12,
                             title = tags$h2(strong("Improving Access to Fish Consumption Advisories and Maintaining Confidence in California's Healthy Seafood Products"))
                             
                         ), # END background info box
                         
                         #background info box ----
                         box(width = 12,
                             title = tagList(strong("Project Background")),
                             HTML("Dichlorodiphenyltrichloroethane (DDT) is an insecticide that is resistant to degradation and can cause increased risks of cancer, premature births, developmental abnormalities, and neurological diseases in humans and animals. A recent <a href='https://www.latimes.com/environment/story/2022-05-18/heres-what-we-know-about-the-legacy-of-ddt-dumping-near-catalina'>rediscovery</a> of a vast barrel field of DDT-laced sludge off the coast of southern California has captured the attention of the public and raised concerns regarding consumption of contaminated seafood. Alongside direct public health impacts, a decrease in seafood consumers poses a threat to the regional economy and recreational fishing communities. This project helps inform the public and give users the autonomy to understand the risk and make informed decisions on their seafood consumption. The interactive element of this application will allow users to access predicted concentrations of total DDT in seafood catch based on their location and the specific species of their catch."),
                             # tags$img(src = "dumpsite.png.jpeg", 
                             #          alt = "Map of fishing zones and the number of fish samples through time, by region (inset). Nearshore 708 polygons are derived from McLaughlin et al. (2021) and pink blocks are California Department of Fish and Game 256 km2 709 fishing blocks.",
                             #          style = "max-width: 90%; display: block; margin: 0 auto;")
                             
                         ) # END background info box
                ),
                         
                tabPanel("Authors", 
                         
                         #background info box ----
                         box(width = 12,
                             title = tagList(strong("Authors")),
                             p("This application was developed as part of a Masters in Environmental Data Science Capstone project as part of the Bren School, for Scripps and CalCOFI. "),
                             p("This project was completed by a group of graduate students at the Bren School of Environmental Science & Management, UC Santa Barbara. Team members include Hope Hahn, Luna Herschenfeld-Catalán, Benjamin Versteeg, and Kate Becker with guidance from our Faculty Advisor Bruce Kendall and Capstone Advisor Carmen Galaz-García.")
                             
                         ) # END author info box 
                         
                         ),
                tabPanel("Data", 
                         
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
                         
                         )
              ))
            
    ), # END about tabItem
    
    # User Manual tabItem ----
    tabItem(tabName = "user_manual",
            
            # column ----
            column(width = 12,
                   

                   # Background Information Box
                   box(
                     width = NULL,
                     title = tagList(strong("Fish DDT Concentration Prediction Dashboard User Manual")),
                     HTML("Welcome to the Fish DDT Concentration Prediction Dashboard! This user-friendly tool is designed to assist fishermen and environmental researchers by predicting DDT concentrations in various fish species based on their geographic catch location. This manual will guide you through initial setup, application operation, and understanding your results.")
                   ), # END background info box 
                   
                   # Section 1: Getting Started
                   box(
                     width = 12,
                     title = tagList(strong("Getting Started")),
                     HTML("Navigate through the application using the tabs at the top of the dashboard. <br><br>
          <strong>Initial Setup:</strong> Begin by identifying your fish species in the 'Fish Identification' tab under 'Resources'. This section provides detailed information about different fish species. <br> <br> Have a good idea of where the fish was caught. Accurately entering the catch location enhances the prediction accuracy.")
                   ), # END section 1: box
          
          # Section 2: Running The Application
          box(
            width = 12,
            title = tagList(strong("Running The Application")),
            HTML("To estimate the DDT concentration:
          <ol>
              <li>Navigate to the <em>'What's In My Catch'</em> tab.</li>
              <li>Select a fish species from the dropdown menu.</li>
              <li>Use the interactive map to place the marker on your fish catch location or manually enter the coordinates. This helps in providing the most accurate prediction.</li>
              <li>Click the <strong>'Predict DDT'</strong> button to receive the forecast.</li>
              <li>Results will be displayed below the map, indicating the predicted DDT concentration.</li>
          </ol>")
          ), # END section 2: box
          
          # Section 3: How To Interpret The Output
          box(
            width = 12,
            title = tagList(strong("How To Interpret The Output")),
            HTML("The output displays the estimated DDT concentration in the fish species at your specified location. This measurement is shown in ng/g lipid units, which reflects the DDT levels typically found in the tissue of the species based on the entered parameters. Understanding these results can help in assessing potential health risks and making informed decisions.")
          ), # END section 3: box
          
          # Section 4: Troubleshooting
          box(
            width = 12,
            title = tagList(strong("Troubleshooting")),
            HTML("Encountering issues? Here are some tips to help you solve common problems:<br>
          <ul>
              <li><strong>Input Validation:</strong> Ensure that the fish species and location details are correctly entered. Verify that the location is geographically plausible for the selected species.</li>
              <li><strong>Map Interaction:</strong> If the map does not respond or the marker does not move, refresh the application or check your internet connection.</li>
          </ul>
          If problems persist, please contact the support team for further assistance.")
          ) # END section 4: box
          

            ), # END left-hand column 

    ), # END User Manual tabItem


# fish_identification tabItem ----
tabItem(tabName = "fish_id",
        
        # left - hand column ----
        column(width = 12,
               
               #background info box ----
               box(width = NULL,
                   title = tagList(strong("Having trouble identifying your fish?")),
                   tags$img(src = "fish-id.png", 
                            alt = "Commerical fishing catches defined by the OEHHA",
                            style = "max-width: 90%; display: block; margin: 0 auto;")
                   
               ) # END background info box 
               
        ) # END left-hand column , # drag mouse over fish and its name will pop up 
        
        
        # fluidRow(width = 6,
        #          useShinyjs(),
        #           extendShinyjs(text = jscode, functions = c("backgroundCol")),
        #           p(id = "name", "My name is Dean"),
        #           p(id = "sport", "I like soccer"),
        #           selectInput(inputId = "col", label = "Colour",
        #                       c("green", "yellow", "red", "blue", "white")),
        #           selectInput(inputId = "selector", label = "Element", c("sport", "name", "button")),
        #           actionButton(inputId = "button", label = "Go"))
        
        
), #END fish_identification tabItem

# research tabItem ----
tabItem(tabName = "research",
        
        # full column ----
        column(width = 12,
               
               #collaboration info box ----
               box(width = NULL,
                   title = tagList(strong("Research and Collaborations")),
                   "Here we can add any future collaborations and research that may be added in the future. This could also be a place to have links to other sites and plug anyone else."
                   
               ) # END collaboration info box 
               
        ), # END column 
        
) # END research tabItem


  ) # END tabItems

) # END dashboardBody 



#..................combine all in dashboardPage..................

dashboardPage(header, sidebar, body, skin = "black")



