#........................dashboardHeader.........................
header <- dashboardHeader(
  title = tags$div(
    style = "position: relative; 
    width: 100%; 
    font-size: 40px;",
    "SaferSeafood"
  ),
  
  titleWidth = 2000
  
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
                       menuSubItem(text = "Research", tabName = "research")
              
  ) # END sidebarMenu
) # end dashboard Sidebar
) 

#..........................dashboardBody.........................
body <-dashboardBody(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), 
  ), 

            
            # # allow for user to use current location
            # tags$script('
            #     $(document).ready(function () {
            #       navigator.geolocation.getCurrentPosition(onSuccess, onError);
            #     
            #       function onError (err) {
            #         Shiny.onInputChange("geolocation", false);
            #       }
            #     
            #       function onSuccess (position) {
            #         setTimeout(function () {
            #           var coords = position.coords;
            #           console.log(coords.latitude + ", " + coords.longitude);
            #           Shiny.onInputChange("geolocation", true);
            #           Shiny.onInputChange("lat", coords.latitude);
            #           Shiny.onInputChange("long", coords.longitude);
            #         }, 1100)
            #       }
            #     });
            #     ')),
  
  

  # about tabItem ----
  tabItems(

    tabItem(tabName = "whats_in_my_catch",
            box(
              width = NULL,
              title = tagList(strong("What's In My Catch?", style = "font-size: 34px;, font-family: Tahoma, Geneva, sans-serif;"))),

            fluidRow(
              column(width = 12,
                     # Add map box with point dragger
                     box(title = span("Caught a fish off the coast of Southern California? Fill the required fields below to better understand the levels of DDT contamination in your catch.", style = "font-size: 16px; font-family: Tahoma, Geneva, sans-serif; ",  width = 12),
                          width = NULL,
                         div(
                           class = "map-container",
                           tags$b("Select location where your fish was caught:", style = "color:#0c3D6E; font-size: 16px;"),
                           leafletOutput(outputId = "locationMap")  # Header below the title but above the map output
                         )
                     ),
                     box(width = NULL,
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
                        
                         #checkboxInput(inputId = "use_location", "Use your current location?"),
                         actionButton("predict_button", "Predict!", class = "btn-primary"),
                         span(textOutput("validation_result"), style = "color:red")
                         ),
              column(width = 6,
                            box(
                              width = NULL,
                              div(class = "prediction-title",
                                  tags$b("Prediction Results", style = "color:#0c3D6E; font-size: 16px;font-family: Tahoma, Geneva, sans-serif;"),  # Prediction results title
                                  div(class = "info-button",
                                      style = "display: flex; align-items: right;",
                                      icon("info-circle", lib = "font-awesome"),  # Info icon
                                      actionButton("info_button", "", style = "display: none;"))),  # Hidden button
                              tags$script(HTML('
                $(document).ready(function(){
                    $(".info-button").click(function(){
                        alert("A serving size is defined by the OEHHA as an 8oz skinless fillet.");
                    });
                });
            ')),
            #status = "warning", 
            solidHeader = TRUE,
            collapsible = TRUE,
            textOutput("prediction"),
            textOutput("serving_size"),
            # tags$div(
            #   style = "display: flex; align-items: center; word-wrap: break-word;",
            #   verbatimTextOutput("serving_size"),
            #   actionButton("info_button", style = "margin-left: 5px;", icon("info-circle"))
            # ),
            #create the serving size the serving size info comes up
            
            bsTooltip(id = "info-button", 
                      title = "A serving size is defined by the OEHHA as an 8oz skinless fillet.")
                            )
                     ),
            column(width = 6,
                   box(
                     width = NULL, 
                     div(class = "prediction-title",
                         tags$b("Other Health Advisories", style = "color:#0c3D6E; font-size: 16px;")), 
                     #status = "success", 
                     solidHeader = FALSE,
                     collapsible = FALSE,
                     textOutput(outputId = "advisory"),
                     #imageOutput(outputId = "advisory_image"),
                     tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"
                     )
                   )
            )
              )
                     
                     
#                      box(width = NULL,
#                          
#                          div(class = "prediction-title",
#                              tags$b("Prediction Results", style = "color:#0c3D6E; font-size: 16px;font-family: Tahoma, Geneva, sans-serif;"),  # Prediction results title
#                              div(class = "info-button",
#                                  style = "display: flex; align-items: right;",
#                                  icon("info-circle", lib = "font-awesome"),  # Info icon
#                                  actionButton("info_button", "", style = "display: none;"))),  # Hidden button
#                          tags$script(HTML('
#                             $(document).ready(function(){
#                               $(".info-button").click(function(){
#                                 alert("A serving size is defined by the OEHHA as an 8oz skinless fillet.");
#                               });
#                             });
#                           ')),
#                          #status = "warning", 
#                          solidHeader = TRUE,
#                          collapsible = TRUE,
#                          textOutput("prediction"),
#                          textOutput("serving_size"),
#                          # tags$div(
#                          #   style = "display: flex; align-items: center; word-wrap: break-word;",
#                          #   verbatimTextOutput("serving_size"),
#                          #   actionButton("info_button", style = "margin-left: 5px;", icon("info-circle"))
#                          # ),
#                          #create the serving size the serving size info comes up
#                          
#                          bsTooltip(id = "info-button", 
#                                    title = "A serving size is defined by the OEHHA as an 8oz skinless fillet.")
#                          
#                      ),
#                      
# #----------------------------------------------------------------------------------------------                     
#                      box(
#                        width = NULL, 
#                        div(class = "prediction-title",
#                            tags$b("Other Health Advisories", style = "color:#0c3D6E; font-size: 16px;")), 
#                          #status = "success", 
#                          solidHeader = FALSE,
#                          collapsible = FALSE,
#                          textOutput(outputId = "advisory"),
#                          #imageOutput(outputId = "advisory_image"),
#                          tags$style(type="text/css",
#                                     ".shiny-output-error { visibility: hidden; }",
#                                     ".shiny-output-error:before { visibility: hidden; }"
#                          )
#                      )
# #-------------------------------------------------------------------------------------------------
#                      )
            
            # column(width = 4,
            #          
            #          box(width = NULL,
            #              title = tagList("Prediction Result"),
            #              status = "success", 
            #              solidHeader = TRUE,
            #              collapsible = TRUE,
            #              verbatimTextOutput("prediction"),
            #              tags$div(
            #                style = "display: flex; align-items: center; word-wrap: break-word;",
            #                verbatimTextOutput("serving_size"),
            #                actionButton("info_button", style = "margin-left: 5px;", icon("info-circle"))
            #              ),
            #              # create tooltip so that if you hover over the serving size the serving size info comes up
            #              bsTooltip(id = "info_button", 
            #                        title = "A serving size is defined by the OEHHA as an 8oz skinless fillet.")
            #              
            #          ),
            #          box(width = NULL, 
            #              title = "Health Advisories", status = "success", solidHeader = TRUE,
            #              collapsible = TRUE,
            #              imageOutput(outputId = "advisory_image")
            #          )
            # ) # end map row
            
    )), # END what's in my catch tab item
    
    # about tabItem ----
    tabItem(tabName = "about",
            fluidRow(
              tabBox(
                title = NULL, width = NULL,
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "about", height = "250px",
                tabPanel("Project Background",
                         
                         #title box ----
                         box(width = NULL,
                             title = tags$h2(strong("Improving Access to Fish Consumption Advisories and Maintaining Confidence in California's Healthy Seafood Products"))
                             
                         ), # END background info box
                         
                         #background info box ----
                         box(width = NULL,
                             title = tagList(strong("Project Background")),
                             HTML("Dichlorodiphenyltrichloroethane (DDT) is an insecticide that is resistant to degradation and can cause increased risks of cancer, premature births, developmental abnormalities, and neurological diseases in humans and animals. A recent <a href='https://www.latimes.com/environment/story/2022-05-18/heres-what-we-know-about-the-legacy-of-ddt-dumping-near-catalina'>rediscovery</a> of a vast barrel field of DDT-laced sludge off the coast of southern California has captured the attention of the public and raised concerns regarding consumption of contaminated seafood. Alongside direct public health impacts, a decrease in seafood consumers poses a threat to the regional economy and recreational fishing communities. This project helps inform the public and give users the autonomy to understand the risk and make informed decisions on their seafood consumption. The interactive element of this application will allow users to access predicted concentrations of total DDT in seafood catch based on their location and the specific species of their catch.")
                             ) #END background box
                         
                          # box(width = 12,
                          #     title = NULL),
                          #"SaferSeafood is an educational tool to inform people about contamination in Seafood",
                          
                          # END disclaimer box 
    
                             # tags$img(src = "dumpsite.png.jpeg", 
                             #          alt = "Map of fishing zones and the number of fish samples through time, by region (inset). Nearshore 708 polygons are derived from McLaughlin et al. (2021) and pink blocks are California Department of Fish and Game 256 km2 709 fishing blocks.",
                             #          style = "max-width: 90%; display: block; margin: 0 auto;")
                             
                ),
                         
              
                tabPanel("Authors", 
                         
                         #background info box ----
                         box(width = NULL,
                             title = tagList(strong("Authors")),
                             p("This application was developed as part of a Masters in Environmental Data Science Capstone project as part of the Bren School, for Scripps and CalCOFI. "),
                             p("This project was completed by a group of graduate students at the Bren School of Environmental Science & Management, UC Santa Barbara. Team members include Hope Hahn, Luna Herschenfeld-Catalán, Benjamin Versteeg, and Kate Becker with guidance from our Faculty Advisor Bruce Kendall and Capstone Advisor Carmen Galaz-García.")
                             
                         ) # END author info box 
                         
                         ),
                tabPanel("Data", 
                         
                         #right - hand column ----
                         fluidRow(
                                  
                                  # data source box ----
                                  
                                  box(width = NULL,
                                      title = tagList(strong("The Data")),
                                      "All resources employed in this study are provided by the client, Scripps Institute of Oceanography and California Cooperative Oceanic Fisheries Investigations (CalCOFI). Raster and tabular data collected across the Southern California Bight will be used to conduct this analysis."
                                      
                                  ), #END data source box
                                  
                                  box(width = NULL,
                                      
                                      title = tagList(strong("Citation")),
                                      "All of the data used for this project has been collected from public data files, and all code and future data/modeling will be available publicly through the team's GitHub organization and repositories. All statistical and web application coding will be conducted in R within RStudio, so any interested parties will be able to reproduce any work in R. "
                                      
                                      
                                  ), #END data source box 
                                  
                                  #disclaimer box ----
                                  box(width = NULL,
                                      
                                      title = tagList(strong("Disclaimer")),
                                      "There are no restrictions on sharing the data and any outputs that result from this project, but all sources of data should be referenced.",
                                      
                                      #style = "background-color: #ff0000;"  # change color as needed
                                      
                                      
                                  ) # END disclaimer box
                                )
                         
                         )
              ))
            
    ), # END about tabItem
    
    # User Manual tabItem ----
    tabItem(tabName = "user_manual",
            fluidRow(
              tags$style(HTML(".container-fluid { background-color: #ffffff; }")),
            
            
            # column ----
            column(width = 12,
                   
                   fluidRow(
                   # Background Information Box
                   box(
                     width = NULL,
                     title = tagList(strong("Fish DDT Concentration Prediction Dashboard User Manual")),
                     HTML("Welcome to the Fish DDT Concentration Prediction Dashboard! This user-friendly tool is designed to assist fishermen and environmental researchers by predicting DDT concentrations in various fish species based on their geographic catch location. This manual will guide you through initial setup, application operation, and understanding your results.")
                   ) # END background info box 
                   ),
                   
                   fluidRow(
                   # Section 1: Getting Started
                   box(
                     width = 12,
                     title = tagList(strong("Getting Started")),
                     HTML("Navigate through the application using the tabs to the right of the dashboard. <br><br>
          <strong>Initial Setup:</strong> Begin by identifying your fish species in the 'Fish Identification' tab under 'Resources'. This section provides detailed information about different fish species. <br> <br> Have a good idea of where the fish was caught. Accurately entering the catch location enhances the prediction accuracy.")
                   ) # END section 1: box
                   ),
          
                   fluidRow(
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
              <li>Results will be displayed below the map.</li>
          </ol>")
          ) # END section 2: box
                   ),
                   
                   fluidRow(
          # Section 3: How To Interpret The Output
          box(
            width = 12,
            title = tagList(strong("How To Interpret The Output")),
            HTML("The output displays the estimated DDT concentration in the fish species at your specified location. This measurement is shown in ng/g lipid units, which reflects the DDT levels typically found in the tissue of the species based on the entered parameters. Along with the DDT concentration, a recommending serving size and relevent mercury advisories are also outputted. Understanding these results can help in assessing potential health risks and making informed decisions.")
          ) # END section 3: box
                   ),
          
                   fluidRow(
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
                   )
          

            ), # END left-hand column 
)
    ), # END User Manual tabItem


# fish_identification tabItem ----
tabItem(tabName = "fish_id",
        
        box(
          width = NULL,
          title = tagList(strong("Having trouble identifying your fish?")),
        div(class = "well",
            p("Use the link below to access the California Marine Species Portal, where you can find detailed information about various fish species. This resource may assist you in identifying the fish species by their common names, scientific names, and visual characteristics."),
            tags$a(href = "https://marinespecies.wildlife.ca.gov/", target = "_blank", 
                   class = "btn btn-primary", "Visit the California Marine Species Portal"),
            br(), br(),
            p("Instructions:"),
            tags$ol(
              tags$li("Use the search bar to enter the common name or scientific name of the fish."),
              tags$li("Use the filters on the left to narrow down by category, group, region, or gear type."),
              tags$li("Click on any fish entry to get more detailed information including photos and distinctive features.")
            ),
            p("These features will help you to effectively identify the fish species you encounter.")
            # ,
            # img(src = "www/help-guide-image.png", height = "200px", alt = "Helpful Guide Image")
        ),
        p("For additional assistance, please refer to the tutorial videos and FAQs on the portal."),
        div(class = "well",
            p("Still having trouble? Contact our support team for personalized help."),
            tags$a(href = "bversteeg@ucsb.edu", class = "btn btn-success", "Email Support")
        ))
        
        
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

  ),# END tabItems

 # end body


# 
# 
# #.........................dashboardfooter.........................
# footer <- tags$footer("", align = "bottom", style = "
#               position: absolute;
#               width:100%;
#               height:100px;",
#               tags$img(src = "white-scripps-logo.png",
#                        style = "position: absolute; width: 300px; height: auto;"),
#               tags$img(src = "calcofi-logo.png",
#                        style = "position: absolute; width: 70px; height: auto;"),
#               tags$img(src = "bren-white-logo.png",
#                        style = "position: absolute; width: 250px; height: auto;")
# )
# )
# 
# 
# #..................combine all in dashboardPage..................
# 
# dashboardPage(header, sidebar, body, footer, skin = "black")



#.........................dashboardfooter.........................
#  footer <- tags$footer("", align = "bottom", style = "
#                position: absolute;
#                width:100%;
#                height:100px;
#                display: flex;",
#                tags$img(src = "white-scripps-logo.png",
#                         style = "position: absolute; bottom: 25px; right : 575px; width: 300px; height: auto;"),
#                tags$img(src = "calcofi-logo.png",
#                        style = "position: absolute; bottom: 10px; right: 900px; width: 70px; height: auto;"),
#               tags$img(src = "bren-white-logo.png",
#                        style = "position: absolute; bottom: 25px; right: 300px; width: 250px; height: auto;")
# )
# )


footer <- tags$footer(
  style = "background-color: #0c3D6E;
  position: absolute;
  bottom: 0;
  width: 90%;
  height: 55px;
  display: flex;
  align-items: center;
  justify-content: space-around;",
  tags$img(src = "white-scripps-logo.png",
           style = "width: 300px; height: auto;"),
  tags$img(src = "calcofi-logo.png",
           style = "width: 70px; height: auto;"),
  tags$img(src = "bren-white-logo.png",
           style = "width: 250px; height: auto;")
)
)





#..................combine all in dashboardPage.............s.....

dashboardPage(header, sidebar, body, footer, skin = "black")











