#........................dashboardHeader.........................
header <- dashboardHeader(
  title = tags$div(
    style = "position: relative; 
    width: 100%; 
    font-size: 45px;,
    font-family: Tahoma, Geneva, sans-serif;",
    "SaferSeafood"
  ),
  
  titleWidth = 2000
  
) # END dashboardHeader

#........................dashboardSidebar........................

sidebar <- dashboardSidebar(
  
  #sidebarMenu ----
  sidebarMenu(menuItem(text = "Toxin Tracker", tabName = "whats_in_my_catch"), 
              menuItem(text = "Help", tabName = "help",
                       menuSubItem(text = "User Manual", tabName = "user_manual"),
                       menuSubItem(text = "Troubleshooting", tabName = "troubleshooting")),
              menuItem(text = "About", tabName = "about"), 
              menuItem(text = "Resources", tabName = "resources", #
                       menuSubItem(text = "Fish Identification", tabName = "fish_id")#,
                       #menuSubItem(text = "DDT Research", tabName = "research")
              ),
              # Add logos to sidebar 
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
  ) # End dashboard Sidebar
) 

#..........................dashboardBody.........................
body <-dashboardBody(
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), 
  ), 
  
  fluidPage(
    
    
    # Toxin Tracker tabItem ----
    tabItems(
      
      tabItem(tabName = "whats_in_my_catch",
              box(
                width = NULL,
                title = tagList(strong("DDT Advisory For You and Your Seafood", style = "font-size: 34px;, font-family: Tahoma, Geneva, sans-serif;")),
                HTML("<br>"), 
                HTML("<div style='text-align: center;'><span style='font-size: 20px;'>Caught a fish off the coast of Southern California? Fill in the required fields below to understand the DDT levels in your seafood and receive serving size recommendations based on DDT, PCBs, and mercury advisories.</span></div>"),
                HTML("<br>"), 
                HTML("<div style='text-align: center;'><span style='font-size: 12px;'>Disclaimer: This research project was designed for educational purposes. The information provided here does not come from any public agency and we are not making health recommendations.</span></div>")
                
              ), 
              
              
              fluidRow(
                column(width = 12,
                       # Add map box with point 
                       box(
                         width = NULL,
                         div(
                           class = "map-container",
                           tags$b("Step 1: Click the location on the map where your fish was caught within the study area (outlined in blue)", style = "color:#f2570f; font-size: 20px;"),
                           HTML("<br> <br>"), 
                           
                           tags$div(tags$b("Step 2: Please select your catch species in the dropdown below the map"), 
                                    style = "font-size: 20px; color:#f2570f;"),
                           
                           HTML("<br>"), 
                           HTML("<div style='text-align: center;'><span style='font-size: 16px;'>Click through map layers to gather more information</span></div>"),
                           leafletOutput(outputId = "locationMap"),# Header below the title but above the map output
                           HTML("<span style='color: black; font-size: 12px;'>Privacy Statement: No data shared with us will be given to third parties or stored in any way. Your data will never be used by us for any purpose other than DDT concentration predictions.</span>")
                         )
                       ),
                       
                       box(width = 6,
                           height = "185px",
                           tags$style(HTML(".selectize-control.single .selectize-input {
                     font-size: 16px; 
                   }")),
                   
                   
                   selectizeInput(inputId = "species", 
                                  label = tags$span("Select Species:", style = "font-size: 16px;"),
                                  choices = sort(str_to_title(fish_lh$CompositeCommonName)),
                                  options = list(
                                    placeholder = 'Please select a species',
                                    onInitialize = I('function() { this.setValue(""); }')
                                  )
                   ),
                   HTML("<div><span style='color: black; font-size: 14px;'><b>Need help identifying your catch? Check out this <a href='https://marinespecies.wildlife.ca.gov'>resource!</a></b></span></div>"), 
                   
                   HTML("<br>"), 
                   #checkboxInput(inputId = "use_location", "Use your current location?"),
                   actionButton("predict_button", "Predict!", class = "btn-primary")
                       ),
                   
                   
                   
                   
                   column(width = 6,
                          box(
                            width = NULL, 
                            height = "185px", # Adjust the height here
                            div(class = "species-title",
                                tags$b("Image of Species", style = "color:#0c3D6E; font-size: 16px;")), 
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
                   
                   fluidRow(
                     column(width = 6,
                            box(
                              width = NULL,
                              height = "260px",
                              div(class = "prediction-title",
                                  tags$b("DDT Prediction Results *", style = "color:#0c3D6E; font-size: 16px;font-family: Tahoma, Geneva, sans-serif;"), 
                                  tags$br(),
                                  tags$b("DDT Prediction Results consider only the effects of DDT.", style = "color:0c3D6x; font-size: 12px;")                              ),  
                              #status = "warning",
                              
                              solidHeader = TRUE,
                              collapsible = FALSE,
                              HTML("<br>"), 
                              
                              span(textOutput("validation_result"), style = "color:red"),
                              span(textOutput("fish_error"), style = "color:red"),
                              span(textOutput("prediction"), style = "color:black"),
                              HTML("<br>"), 
                              span(htmlOutput("serving_size"), style = "color:black"),
                              HTML("<br>"), 
                              #plotOutput(outputId = "servings", height = "100px"),  # Adding the plot output here
                              
                              
                              
                              # div(class = "info-button",
                              #     style = "display: flex; align-items: right;",
                              #     icon("info-circle", lib = "font-awesome"),  # Info icon
                              #     actionButton("info_button", "", style = "display: none;")),
                              # # Hidden button
                              # tags$script(HTML('
                              #     $(document).ready(function(){
                              #         $(".info-button").click(function(){
                              #             alert("A serving size is defined by the OEHHA as an 8oz skinless fillet.");
                              #         });
                              #     });
                              # ')),        
                            ),),
                     
                     
                     
                     column(width = 6,
                            box(
                              width = NULL, 
                              height = "260px",
                              div(class = "prediction-title",
                                  tags$b("OEHHA Health Advisory **", style = "color:0c3D6x; font-size: 16px;"), 
                                  tags$br(),
                                  tags$b("OEHHA Health Advisories and Safe Eating Guidelines consider the cumulative effects of DDT, PCBs, and mercury.", style = "color:0c3D6x; font-size: 12px;")), 
                              #status = "success", 
                         HTML("<br>"), solidHeader = FALSE,
                              collapsible = FALSE,
                              span(htmlOutput(outputId = "advisory"), style = "color:black"),
                              #imageOutput(outputId = "advisory_image"),
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              #          div(class = "info-button2",
                              #              style = "display: flex; align-items: right;",
                              #              icon("info-circle", lib = "font-awesome"),  # Info icon
                              #              actionButton("info_button", "", style = "display: none;")),
                              #          
                              #          
                              #          # Hidden button
                              #          tags$script(HTML('
                              #     $(document).ready(function(){
                              #         $(".info-button2").click(function(){
                              #             alert("This advisory was provided from the OEHHA Fish Advisory webpage.");
                              #         });
                              #     });
                              # '))
                            )
                            
                     ),
                     # column(
                     #   width = 12,
                     #   HTML("<div><span style='position: top: 0; color: #f2570f; font-size: 18px;'><b>Follow the lower recommendation for number of servings per week.</b></span></div>"),
                     #   HTML("<br>")
                     # ),
                     column(
                       width = 12,
                       box(
                         width = NULL,
                         HTML("<div><span style='position: top: 0; color: #f2570f; font-size: 18px;'><b>Following the lower recommendation, your recommended maximum number of servings per week for this species and location is below. </b></span></div>"),
                         HTML("<br>"),
                         HTML("<div><span style='position: top: 0; color: #f2570f; font-size: 12px;'><b>If two serving sizes are shown, the lower is for women 18-49 years and children 1-17 years and the larger is for women 50 years and older and men 18 years and older. </b></span></div>"),
                         HTML("<br>"),
                         style = "text-align: center;",  # Center-align the source wording
                         plotOutput(outputId = "servings", height = "100px"),  # Adding the plot output here
                         
                       ), # END serving size box
                       
                       
                       
                       HTML("<br>")
                       
                     ), # END serving size REDO
                     column(
                       width = 12,
                       box(
                         width = NULL,
                         #HTML("<div><span style='position: top: 0; color: #f2570f; font-size: 18px;'><b>Follow the lower recommendation for number of servings per week.</b></span></div>"),
                         #HTML("<br>"), 
                         # Apply CSS styling to the image tag
                         tags$img(src = "fish-serving-box.png",
                                  alt = "Fish Hand Image",
                                  style = "max-width: 100%; max-height: 100%;"),
                         # Adding the source wording below the image
                         tags$p("Source: ",
                                tags$a(href = "https://oehha.ca.gov/advisories/statewide-advisory-eating-fish-california-coastal-locations-without-site-specific-advice", "OEHHA")),
                         style = "text-align: center;"  # Center-align the source wording
                       ), # END serving size box
                       
                       HTML("<div><span style='color: black; font-size: 12px;'>*  A serving size is defined by the OEHHA as an 8oz skinless fillet</span></div>"),# Prediction results title
                       
                       HTML("<div><span style='color: black; font-size: 12px;'>** This advisory was provided from the OEHHA Fish Advisory webpage</span></div>"),
                       
                       
                       HTML("<br>")
                       
                     ), # END serving size column
                     
                     
                   ) # END prediciton results box 
                   
                ) # END prediction results fluidRow 
                
              ) # END what's in my catch fluidROW
              
      ), # END what's in my catch tab item
      
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
                           height = "170px",
                           title = tags$h2(strong("Dashboard User Manual")),
                           HTML("Welcome to the Fish DDT Concentration Prediction Dashboard! This user-friendly tool is designed to assist fishermen and environmental researchers by predicting DDT concentrations in various fish species based on their geographic catch location and catch species. This manual will guide you through initial setup, application operation, and understanding your results.")
                         ) # END background info box 
                       ), # END fluidRow 
                       
                       fluidRow(
                         # Section 1: Getting Started
                         box(
                           width = 12,
                           title = tagList(strong("Getting Started")),
                           HTML("Navigate through the application using the tabs to the left of the dashboard. <br><br>
          <strong>Initial Setup:</strong> Begin by identifying your fish species in the 'Fish Identification' tab under 'Resources'. This platform provides detailed information about a wide range of California marine species. <br> <br> Have a good idea of where the fish was caught. Accurately entering the catch location enhances the prediction accuracy. This study focuses on a particular area with defined bounds so be sure that your location falls within the specified study area for the app to work effectively.")
                         ) # END section 1: box
                       ), # END fluidRow for getting started 
          
          fluidRow(
            # Section 2: Running The Application
            box(
              width = 12,
              title = tagList(strong("Running The Application")),
              HTML("To estimate the DDT concentration:
          <ol>
              <li>Navigate to the <em>'Toxin Tracker'</em> tab.</li>
              <li>Use the interactive map to select your fish catch location by placing a marker. This helps in providing the most accurate predictions. Your location must be within the highlighted study area in order to receive a prediction.</li>
              <li>Select a fish species from the dropdown menu.</li>
              <li>Click the <strong>'Predict DDT'</strong> button to receive the forecast.</li>
              <li>Results will be displayed below the map.</li>
          </ol>")
            ) # END section 2: box
          
          ), # END fluidRow for Running the Application 
          
          fluidRow(
            # Section 3: How To Interpret The Output
            box(
              width = 12,
              title = tagList(strong("How To Interpret The Output")),
              HTML("The output displays the estimated DDT concentration in the fish species at your specified location. This measurement is shown in ng/g units, which reflects the DDT levels typically found in the tissue of the species based on the entered parameters. Along with the DDT concentration, a recommended serving size and relevant OEHHA consumption advisory are also outputted. Understanding these results can help in assessing potential health risks and making informed decisions. These results do not come from any federal agency and should be used in conjunction with advisories provided by the California Office of Environmental Health Hazard Assessment, the Food and Drug Administration, and the Fish Contamination Education Collaborative. These are NOT FDA sectioned advisories. For women and children related advice refer to this link: <a href='https://oehha.ca.gov/fish/women-and-children'>https://oehha.ca.gov/fish/women-and-children</a>" )
              
            ) # END section 3: box
            
          ), # End how to interpret output FluidRow
          
                ) # END fluidRow for troubleshopping 
          
              ) # END fluidRow for User Manual
          
      ), # END User Manual tabItem
      
      # Troubleshooting tabItem ----
      tabItem(tabName = "troubleshooting",
              fluidRow(
                tags$style(HTML(".container-fluid { background-color: #ffffff; }")),
                
                
                # column ----
                column(width = 12,
                       
                       fluidRow(
                         # Title Box
                         box(
                           width = NULL,
                           height = "100px",
                           title = tags$h2(strong("Troubleshooting"))
                         ) # END Title box 
                       ), # END fluidRow 
                       
                       fluidRow(
                         
                         box(
                           width = 12,
                           title = tagList(strong("Potential Map Display Errors")),
                           HTML("<strong>Symptom:</strong> Map does not load or markers do not appear.<br><br>
<strong>Possible Causes:</strong> Browser cache issues, network issues, or compatibility issues on specific devices.<br><br>
<strong>Resolution:</strong> Refresh the page to clear the cache. Check your internet connection. If the issue persists on a specific device, try accessing the dashboard from a different device or browser as a temporary workaround. If the problem continues, please contact the support team for further assistance.<br><br>
<strong>Note:</strong> Compatible browsers for the leaflet package include:<br>
- Desktop: Chrome, Firefox, Safari 5+, Opera 12+, IE 9-11, Edge <br>
- Mobile: Safari for iOS 7+, Chrome for mobile, Firefox for mobile, IE10+ for Win8 devices")
                         ) # END box
                       ), # END fluidRow 

fluidRow(
  
  box(
    width = 12,
    title = tagList(strong("Potential Data Errors")),
    HTML("<strong>Symptom:</strong> Incorrect or no inputs displayed after inputting location and species.<br><br>
<strong>Possible Causes:</strong> Misidentification of species, incorrect location coordinates, network issues, server-side problems.<br><br>
<strong>Resolution:</strong> Ensure that the fish species and location details are correctly entered. Verify that the location is geographically plausible for the selected species and study area. Check your internet connection. If the problem continues, please contact the support team for further assistance. <br><br>
<strong>Note:</strong> Some potential server-side problems include high load, misconfiguration, crashes, or maintenance downtime.")
  ) # END box

), # END fluidrow 

fluidRow(
  
  box(
    width = 12,
    title = tagList(strong("Error Messages")),
    HTML('<strong>Location Error Message:</strong> We’re sorry, but we are not able to make predictions for that location. We can only make valid predictions for the area outlined on the map."<br>
    <strong>-</strong> Indicates that the location selected is not a valid location and no prediction will be made until a valid location is selected.<br><br>
                   
<strong>Fish Error Message:</strong> "Please select a fish species before pressing the Predict button."<br>
   <strong>-</strong> Triggered when a user clicks the predict button without first selecting a fish species. No prediction will be made until a species is selected.<br><br>

<strong>Advisory Error Message:</strong> "OEHHA has not yet assessed your species at your location."<br>
    <strong>-</strong> Specifies that there are no current advisories for that species of fish in the selected location.
')

  ) # END box

), # End FluidRow

fluidRow(
  
  box(
    width = 12,
    title = tagList(strong("Still Having Trouble?")),
    HTML('For additional assistance, please refer to the tutorial videos and FAQs on the portal or contact our support team for personalized help.<br><br>'),
    tags$a(href = "mailto:lmmcgill@ucsd.edu", class = "btn btn-success", "Email Support")
  

  ) # END box

), # End FluidRow

                ) # END fluidRow for troubleshopping 

              ) # END fluidRow for Troubleshooting

      ), # END troubleshooting tabItem


# About tabItem ----
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
                         HTML("Dichlorodiphenyltrichloroethane (DDT) is an insecticide that is resistant to degradation and can cause increased risks of cancer, premature births, developmental abnormalities, and neurological diseases in humans and animals. 
                              From 1947-1971 the ocean off the coast of Los Angeles was a dumping ground for the nation's largest producer of DDT. Industrial waste was discharged both through the Los Angeles County wastewater treatment plant, which was deposited nearshore on the Palos Verdes Shelf (PVS), and via ships that transported and dumped bulk waste in deeper waters.
                              The recent <a href='https://www.latimes.com/environment/story/2022-05-18/heres-what-we-know-about-the-legacy-of-ddt-dumping-near-catalina'>rediscovery</a> of offshore dumpsites off the coast of southern California has captured the attention of the public and raised concerns regarding consumption of contaminated seafood."),
                         p(),
                         HTML("The California Environmental Protection Agency Office of Environmental Health Hazard Assessment (OEHHA) currently issues <a href='https://oehha.ca.gov/advisories/statewide-advisory-eating-fish-california-coastal-locations-without-site-specific-advice'>a general statewide consumption advisory based on DDT, PCBs, and mercury for coastal locations and a limited number of site-specific consumption advisories</a>, with a subset of commonly-caught species considered in these advisories. 
                           In order to improve consumption advisory accessibility and generate more localized predictions of DDT risk, SaferSeafood has partnered with Scripps Institution of Oceanography and the California Cooperative Oceanic Fisheries Investigations, who have collated fish and sediment monitoring data to understand the extensive human and ecological impacts resulting from legacy DDT dumping. 
                           Their current model examines factors that drive localized DDT risk in sport fish off the Southern California coast, and results were recently published in the journal <a href='https://www.pnas.org/doi/10.1073/pnas.2401500121'> Proceedings of the National Academy of Sciences </a>."),
                         p(),
                              p("The goal of this project, SaferSeafood, was to expand on this effort by creating statistical models that predict fish DDT concentrations using factors like average sediment DDT concentration, capture year, and species-specific ecological characteristics such as diet and habitat. 
                                This project aims to educate the public and give users the autonomy to understand the risk and make informed decisions on their seafood consumption. 
                                The interactive element of this application allows users to access predicted concentrations of total DDT in seafood catch based on their location and the species of their catch. 
                                Official government consumption advisories are also be provided to the user sourced from OEHHA. 
                                It should be emphasized that this dashboard is a research project designed to educate and inform. 
                                The information provided here does not come from any public agency and we are not making health recommendations.")
                     ),
                     
                     box(width = NULL,
                         title = tagList(strong("Authors")),
                         p("This application was developed as a Masters in Environmental Data Science Capstone project for the Scripps Institute of Oceanography and the California Cooperative Oceanic Fisheries Investigation."),
                         p("This project was completed by a group of graduate students at the Bren School of Environmental Science & Management, UC Santa Barbara. Team members include Hope Hahn, Luna Herschenfeld-Catalán, Benjamin Versteeg, and Kate Becker with guidance from our Faculty Advisor Bruce Kendall and Capstone Advisor Carmen Galaz-García.")
                         
                     )), # END author info box
            
            
            tabPanel("Data", 
                     
                     #right - hand column ----
                     fluidRow(
                       
                       # data source box ----
                       
                       box(width = NULL,
                           title = tagList(strong("The Data")),
                           HTML("All data employed in the up-to-date version of this dashboard was collected by the Southern California Bight Regional Monitoring Program and provided by Scripps Institute of Oceanography as well as California Cooperative Oceanic Fisheries Investigations (CalCOFI). All rasters were processed by Dr. Lillian McGill at the Scripps Institute of Oceanography and the data used for this project was publicly available to us on Dr. Lillian McGill’s GitHub repository.  The data includes four comprehensive databases:  Sediment Data, Sediment Raster’s, DDT Monitoring Data, and Species Life History Characteristics. All data points were collected in the coastal waters of the Southern California Bight, a stretch of coastline that extends more than 600 km from the United States – Mexico border northwards to Point Conception. The metadata can be found in the tfotalDDX_fish_metadata.csv and the totalDDX_fish_southernCA.csv. All of the data used for this project has been collected from public data files, and all code and future data/modeling iswill be available publicly through the team's GitHub organization and repositories. All statistical and web application coding will be conducted in R within RStudio, so any interested parties will be able to reproduce any work in R."),
                           HTML("<br><br>"), 
                           HTML("<div style='text-align: center;'><span style='color: black; font-size: 14px;'>Access data <a href='https://doi.org/10.5061/dryad.7pvmcvf2g'>here!</a></span></div>"),
                           HTML("<br><br>"), 
                           HTML("<div style='text-align: center;'><span style='color: black; font-size: 14px;'>A more detailed description of the data used in this project can be found in the SaferSeafood <a href='https://github.com/SaferSeafood/Shiny-Dashboard'>Github!</a></span></div>")
                           
                           # 
                           # HTML("<div style='text-align: center;'><span style='font-size: 18px;'>From catch to consumption, stay informed regarding the levels of contamination in your fish</span></div>"),
                           
                       ), #END data source box
                       
                       box(width = 12,
                           
                           title = tagList(strong("Citation")),
                           "All of the data used for this project has been collected from public data files, and all code and future data/modeling will be available publicly through the team's GitHub organization and repositories. All statistical and web application coding will be conducted in R within RStudio, so any interested parties will be able to reproduce any work in R. ",
                           p(),
                           p("This application can be cited using the following:"),
                           p("Becker, K., Catalán, L., Hahn, H., Versteeg, B. (2024). SaferSeafood (Version 1.0) [Mobile App]. R package version 4.3.1. https://shiny.calcofi.io/saferseafood")
                           
                       ), #END data source box 
                       
                       #disclaimer box ----
                       box(width = 12,
                           
                           title = tagList(strong("Disclaimer")),
                           "There are no restrictions on sharing the data and any outputs that result from this project, but all sources of data should be referenced.",
                           p(),
                           p("License: The data used in this project is is free and public domain. There are no restrictions on downloaded data.")
                           
                           #style = "background-color: #ff0000;"  # change color as needed
                           
                           
                       ) # END disclaimer box
                     ) # END Data fluidRow
                     
            ) # END data panel 
          ) # END tab box
        ) # END about fluidrow
        
), # END about tabItem



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

        ) # End Fish Identification box 
        
        
), #END fish_identification tabItem

# research tabItem ----
tabItem(tabName = "research",
        
        # full column ----
        column(width = 12,
               
               #collaboration info box ----
               box(width = NULL,
                   title = tagList(strong("The Broader Picture")),
                   p("Ongoing research and educational collaborators, working to understand the human and ecological impacts of DDT witin Southern California, are listed below. 
                     We aim to provide a platform that enables researchers to connect with communities in a meaningful way. If you would like a link added here, please contact the website administrator.")
                   
               ) # END collaboration info box 
               
        ), # END column 
        
) # END research tabItem

    ) # END tabItems

  ) # END fluid page 

)   # END dashboard body



#..................combine all in dashboardPage.............s.....

dashboardPage(header, sidebar, body, skin = "black", title = "SaferSeafood")