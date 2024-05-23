#........................dashboardHeader.........................
header <- dashboardHeader(
  title = tags$div(
    style = "position: relative; 
    width: 100%; 
    font-size: 50px;,
    font-family: Tahoma, Geneva, sans-serif;",
    "SaferSeafood"
  ),
  
  titleWidth = 2000
  
) # END dashboardHeader

#........................dashboardSidebar........................

# add a how to use box to homepage 

sidebar <- dashboardSidebar(
  
  #sidebarMenu ----
  sidebarMenu(menuItem(text = "Toxin Tracker", tabName = "whats_in_my_catch"), # previously dashboard
              menuItem(text = "Help", tabName = "user_manual"),
              menuItem(text = "About", tabName = "about"), # previously welcome
              menuItem(text = "Resources", tabName = "resources", # previously seafood_advisory
                       menuSubItem(text = "Fish Identification", tabName = "fish_id"),
                       menuSubItem(text = "DDT Research", tabName = "research")
              ),
              
              tags$div(
                style = "display: flex; flex-direction: column; align-items: center; margin-top: 30px;",
                
                lapply(1:1, function(i) tags$br()),
                tags$img(src = "white-scripps-logo.png", width = "100%"),
                tags$br(),  # Insert line break
                tags$img(src = "calcofi-logo.png", style = "width: 9vw; height: 9vw;"),
                tags$br(),  # Insert line break
                tags$img(src = "bren-white-logo.png", width = "100%")
              )
              
              # END sidebarMenu
  ) # end dashboard Sidebar
) 

#..........................dashboardBody.........................
body <-dashboardBody(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), 
  ), 
  
  fluidPage(
  
  
  # about tabItem ----
    tabItems(
    
      tabItem(tabName = "whats_in_my_catch",
            box(
              width = NULL,
              title = tagList(strong("DDT Advisories For You and Your Seafood", style = "font-size: 34px;, font-family: Tahoma, Geneva, sans-serif;")),
              HTML("<div style='text-align: center;'><span style='font-size: 18px;'>From catch to consumption, stay informed regarding the levels of contamination in your fish</span></div>"),
              HTML("<div style='text-align: center;'><span style='font-size: 12px;'>Disclaimer: This research project was designed for educational purposes. The information provided here does not come from any public agency and we are not making health recommendations.</span></div>")
              
              ), 
            
            
            
            fluidRow(
              column(width = 12,
                     # Add map box with point dragger 
                     box(title = "Caught a fish off the coast of Southern California? Fill the required fields below to better understand the levels of DDT, Mercurcy, and PCBs that may have accumulated in your seafood", style = "font-size: 16px; font-family: Tahoma, Geneva, sans-serif; ",
                         width = NULL,
                         div(
                           class = "map-container",
                           tags$b("Step 1: Drag the marker to the location where your fish was caught within the study area (outlined in white)", style = "color:#0c3D6E; font-size: 20px;"),
                           HTML("<div style='text-align: center;'><span style='font-size: 16px;'>Click through map layers to gather more information</span></div>"),
                           leafletOutput(outputId = "locationMap"),# Header below the title but above the map output
                           HTML("<b><span style='color: green; font-size: 12px;'>Privacy Statement: No data shared with us will be given third parties or stored in any way. Your data will never be used by us for any purpose other than DDT concentration predictions.</span></b>")
                         )
                     ),
                     column(width = 12,
                            box(width = 12,
                                tags$div(tags$b("Step 2: Please select your catch species"), 
                                         style = "font-size: 20px; color:#0c3D6E;"))),
                     

                     
                     box(width = 6,
                         height = "168px",
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
                              height = "168px", # Adjust the height here
                              div(class = "species-title",
                                  tags$b("Photo of Species", style = "color:#0c3D6E; font-size: 16px;")), 
                              #status = "success", 
                              solidHeader = TRUE,
                              collapsible = FALSE,
                              imageOutput(outputId = "fish_image"), #change to fish images
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              )
                            )
                            
                     ),
                     
                     ### Serving size plot
                     column(
                       width = 12,
                       box(
                         width = NULL,
                         solidHeader = FALSE,
                         collapsible = TRUE,
                         plotOutput(outputId = "servings", height = "100px"),  # Adding the plot output here
                         
                         tags$style(
                           type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                         )
                       )
                     ),
                     
                     fluidRow(
                       column(width = 6,
                              box(
                                width = NULL,
                                height = "190px",
                              div(class = "prediction-title",
                                  tags$b("DDT Prediction Results", style = "color:#0c3D6E; font-size: 16px;font-family: Tahoma, Geneva, sans-serif;"),  # Prediction results title
                                  ),  
            #status = "warning",
            
            solidHeader = TRUE,
            collapsible = FALSE,
            textOutput("prediction"),
            textOutput("serving_size"),
            span(textOutput("fish_error"), style = "color:red"),
            
            div(class = "info-button",
                style = "display: flex; align-items: right;",
                icon("info-circle", lib = "font-awesome"),  # Info icon
                actionButton("info_button", "", style = "display: none;")),
            # Hidden button
            tags$script(HTML('
                $(document).ready(function(){
                    $(".info-button").click(function(){
                        alert("A serving size is defined by the OEHHA as an 8oz skinless fillet.");
                    });
                });
            ')),        
            ),),
            
            
            column(width = 6,
                   box(
                     width = NULL, 
                     height = "190px",
                     div(class = "prediction-title",
                         tags$b("Mercury/PCB Health Advisory", style = "color:#0c3D6E; font-size: 16px;")), 
                     #status = "success", 
                     solidHeader = FALSE,
                     collapsible = FALSE,
                     textOutput(outputId = "advisory"),
                     #imageOutput(outputId = "advisory_image"),
                     tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"
                     ),
                     div(class = "info-button2",
                         style = "display: flex; align-items: right;",
                         icon("info-circle", lib = "font-awesome"),  # Info icon
                         actionButton("info_button", "", style = "display: none;")),
                     
                     
                     # Hidden button
                     tags$script(HTML('
                $(document).ready(function(){
                    $(".info-button2").click(function(){
                        alert("This advisory was provided from the OEHHA Fish Advisory webpage.");
                    });
                });
            '))
                   )
                   
            ),
            
      
            column(
              width = 12,
              box(
                width = NULL,
                # Apply CSS styling to the image tag
                tags$img(src = "fish-serving-box.png",
                         alt = "Fish Hand Image",
                         style = "max-width: 100%; max-height: 100%;"),
                # Adding the source wording below the image
                tags$p("Source: ",
                       tags$a(href = "https://oehha.ca.gov/advisories/statewide-advisory-eating-fish-california-coastal-locations-without-site-specific-advice", "OEHHA")),
                style = "text-align: center;"  # Center-align the source wording
              )
            ),

            # column(
            #   width = 12,
            #   box(
            #     width = NULL,
            #     # Apply CSS styling to the image tag
            #     tags$img(src = "fish-hand.png",
            #              alt = "Source: https://www.fda.gov/food/consumers/advice-about-eating-fish",
            #              style = "max-width: 100%; max-height: 100%;")
            #   )),
            #
            # column(
            #   width = 12,
            #   box(
            #     width = NULL,
            #     height = "200px", # Adjust the height here
            #     div(class = "graph-title",
            #         tags$b("Distribution Graph", style = "color:#0c3D6E; font-size: 16px;")),
            #     solidHeader = FALSE,
            #     collapsible = FALSE,
            #     plotOutput("distPlot"),  # Adding the plot output here
            #     tags$style(
            #       type="text/css",
            #       ".shiny-output-error { visibility: hidden; }",
            #       ".shiny-output-error:before { visibility: hidden; }"
            #     )
            #   )
            # ),
            #
            # column(
            #   width = 12,
            #   box(
            #     width = NULL,
            #     "Content of the box below the distribution graph"
            #   )
            # ),

            # column(width = 12,
            #        box(width = NULL))
            
              )
            
            
            
            ))), # END what's in my catch tab item
    
    # about tabItem ----
    tabItem(tabName = "about",
            fluidRow(
              tabBox(
                title = NULL, width = NULL,
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "about", height = "100%",
                tabPanel("Project Background",
                         
                         #title box ----
                         box(width = NULL,
                             title = tags$h2(strong("Improving Access to Fish Consumption Advisories and Maintaining Confidence in California's Healthy Seafood Products"))
                             
                         ), # END background info box
                         
                         #background info box ----
                         box(width = NULL,
                             title = tagList(strong("Project Background")),
                             HTML("Dichlorodiphenyltrichloroethane (DDT) is an insecticide that is resistant to degradation and <strong>can cause increased risks of cancer, premature births, developmental abnormalities, and neurological diseases in humans and animals </strong> . A recent <a href='https://www.latimes.com/environment/story/2022-05-18/heres-what-we-know-about-the-legacy-of-ddt-dumping-near-catalina'>rediscovery</a> of a vast barrel field of DDT-laced sludge off the coast of southern California has captured the attention of the public and raised concerns regarding consumption of contaminated seafood.The DDT dumping in the Southern California coast, specifically, is an important current issue due to there not being enough information about the DDT concentration in the fish caught there. The California Environmental Protection Agency Office of Environmental Health Hazard Assessment (OEHHA) currently issues statewide consumption advisories for coastal communities. However, these advisories are severely limited as they are site and species-specific, covering only two chemicals: Mercury and Polychlorinated biphenyl (PCB)s. In order to improve consumption advisory accessibility, SaferSeafood has partnered with Scripps Institution of Oceanography and the California Cooperative Oceanic Fisheries Investigations, who have collected and analyzed fish and sediment monitoring data to understand the extensive human and ecological impacts resulting from legacy DDT dumping. Their current model accurately predicts localized risk of DDT in sport fish off the Southern California coast. This risk encompasses the potential adverse effects on human health due to exposure to these contaminants (Marjadi et al. 2021). The goal of this project, SaferSeafood, was to expand on this by creating various models that predict fish DDT levels using factors like sediment DDT, capture year, and fish characteristics. These then were used to develop a spatiotemporal statistical model to predict DDT concentrations for species and locations that were not included in the sample collected by Scripps. This project helps inform the public and give users the autonomy to understand the risk and make informed decisions on their seafood consumption. Alongside direct public health impacts, a decrease in seafood consumers poses a threat to the regional economy and recreational fishing communities.  The interactive element of this application will allow users to access predicted concentrations of total DDT in seafood catch based on their location and the specific species of their catch. Advisories for Mercury and PCBs (Polychlorinated biphenyl) consumption will also be provided to the user sourced from the California Office of Environmental Health Hazard Assessment. It should be emphasized that this dashboard is a research project designed to educate and inform. The information provided here does not come from any public agency and we are not making health recommendations.")
                         ),
                         
                         box(width = NULL,
                             title = tagList(strong("Authors")),
                             p("This application was developed as a Masters in Environmental Data Science Capstone project for the Scripps Institute of Oceanography and the California Cooperative Oceanic Fisheries Investigation."),
                             p("This project was completed by a group of graduate students at the Bren School of Environmental Science & Management, UC Santa Barbara. Team members include Hope Hahn, Luna Herschenfeld-Catalán, Benjamin Versteeg, and Kate Becker with guidance from our Faculty Advisor Bruce Kendall and Capstone Advisor Carmen Galaz-García.")
                             
                         )), # END author info box
                        

                
                # tabPanel("Authors", 
                #          
                #          #background info box ----
                         # box(width = NULL,
                         #     title = tagList(strong("Authors")),
                         #     p("This application was developed as a Masters in Environmental Data Science Capstone project for the Scripps Institute of Oceanography and the California Cooperative Oceanic Fisheries Investigation."),
                         #     p("This project was completed by a group of graduate students at the Bren School of Environmental Science & Management, UC Santa Barbara. Team members include Hope Hahn, Luna Herschenfeld-Catalán, Benjamin Versteeg, and Kate Becker with guidance from our Faculty Advisor Bruce Kendall and Capstone Advisor Carmen Galaz-García.")
                         # 
                         # ) # END author info box
                         # 
                # ),
                tabPanel("Data", 
                         
                         #right - hand column ----
                         fluidRow(
                           
                           # data source box ----
                           
                           box(width = NULL,
                               title = tagList(strong("The Data")),
                               ("All data employed in the up-to-date version of this dashboard was collected by the Southern California Bight Regional Monitoring Program and provided by Scripps Institute of Oceanography as well as California Cooperative Oceanic Fisheries Investigations (CalCOFI). All rasters were processed by Dr. Lillian McGill at the Scripps Institute of Oceanography and the data used for this project was publicly available to us on Dr. Lillian McGill’s GitHub repository.  The data includes four comprehensive databases:  Sediment Data, Sediment Raster’s, DDT Monitoring Data, and Species Life History Characteristics. All data points were collected in the coastal waters of the Southern California Bight, a stretch of coastline that extends more than 600 km from the United States – Mexico border northwards to Point Conception. The metadata can be found in the totalDDX_fish_metadata.csv and the totalDDX_fish_southernCA.csv. All of the data used for this project has been collected from public data files, and all code and future data/modeling iswill be available publicly through the team's GitHub organization and repositories. All statistical and web application coding will be conducted in R within RStudio, so any interested parties will be able to reproduce any work in R. A more detailed description of the data used in this project can be found in the SaferSeafood <a>href='https://github.com/SaferSeafood/Shiny-Dashboard'>GitHub</a>.")
                               
                           ), #END data source box
                           
                           box(width = 12,
                               
                               title = tagList(strong("Citation")),
                               "All of the data used for this project has been collected from public data files, and all code and future data/modeling will be available publicly through the team's GitHub organization and repositories. All statistical and web application coding will be conducted in R within RStudio, so any interested parties will be able to reproduce any work in R. "
                               
                               
                           ), #END data source box 
                           
                           #disclaimer box ----
                           box(width = 12,
                               
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
                         title = tagList(strong("Dashboard User Manual")),
                         HTML("Welcome to the Fish DDT Concentration Prediction Dashboard! This user-friendly tool is designed to assist fishermen and environmental researchers by predicting DDT concentrations in various fish species based on their geographic catch location and catch species. This manual will guide you through initial setup, application operation, and understanding your results.")
                       ) # END background info box 
                     ),
                     
                     fluidRow(
                       # Section 1: Getting Started
                       box(
                         width = 12,
                         title = tagList(strong("Getting Started")),
                         HTML("Navigate through the application using the tabs to the left of the dashboard. <br><br>
          <strong>Initial Setup:</strong> Begin by identifying your fish species in the 'Fish Identification' tab under 'Resources'. This platform provides detailed information about a wide range of California marine species. <br> <br> Have a good idea of where the fish was caught. Accurately entering the catch location enhances the prediction accuracy. This study focuses on a particular area with defined bounds so be sure that your location falls within the specified study area for the app to work effectively.")
                       ) # END section 1: box
                     ),
          
          fluidRow(
            # Section 2: Running The Application
            box(
              width = 12,
              title = tagList(strong("Running The Application")),
              HTML("To estimate the DDT concentration:
          <ol>
              <li>Navigate to the <em>'Toxin Tracker'</em> tab.</li>
              <li>Select a fish species from the dropdown menu.</li>
              </li>Confirm that your fish species is correct with the provided photo </li>
              <li>Use the interactive map to drag the marker to your fish catch location. This helps in providing the most accurate predictions. Your location must be within the highlighted study area in order to receive a prediction.</li>
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
              HTML("The output displays the estimated DDT concentration in the fish species at your specified location. This measurement is shown in ng/g units, which reflects the DDT levels typically found in the tissue of the species based on the entered parameters. Along with the DDT concentration, a recommended serving size and relevant Mercury/PCB advisories are also outputted. Understanding these results can help in assessing potential health risks and making informed decisions. These results do not come from any federal agency and should be used in conjunction with advisories provided by the California Office of Environmental Health Hazard Assessment, the Food and Drug Administration, and the Fish Contamination Education Collaborative. These are NOT FDA sectioned advisories. For women and children related advice refer to this link: <a href='https://oehha.ca.gov/fish/women-and-children'>https://oehha.ca.gov/fish/women-and-children</a>" )
              
            ) # END section 3: box
          ),
          
          fluidRow(
            # Section 4: Troubleshooting
            box(
              width = 12,
              title = tagList(strong("Troubleshooting")),
              HTML("Encountering issues? Here are some tips to help you solve common problems:<br>
          <ul>
              <li><strong>Input Validation:</strong> Ensure that the fish species and location details are correctly entered. Verify that the location is geographically plausible for the selected species and study area.</li>
              <li><strong>Map Interaction:</strong> If the map does not respond or the marker does not move, refresh the application or check your internet connection.</li>
          </ul>
          If problems persist, please contact the support team for further assistance.")
            ), # END section 4: box
          
          column(width = 12,
                 box(
                   width = 12))
          
          ) # END left-hand column 
              )
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
            
            
    ), #END fish_identification tabItem
    
    # research tabItem ----
    tabItem(tabName = "research",
            
            # full column ----
            column(width = 12,
                   
                   #collaboration info box ----
                   box(width = NULL,
                       title = tagList(strong("The Broader Picture")),
                       p("Further research and educational collaborators, working to understand the human and ecological impacts of the recently discovered DDT dumpsite, will be highlighted here. We aim to provide a platform that enables researchers to connect with communities in a meaningful way.")
                       
                   ) # END collaboration info box 
                   
            ), # END column 
            
    ) # END research tabItem
    
  )# END tabItems
)
)

  # end body


# href css

#..................combine all in dashboardPage.............s.....

dashboardPage(header, sidebar, body, skin = "black", title = "SaferSeafood")