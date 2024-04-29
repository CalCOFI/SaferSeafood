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
            fluidRow(width = NULL,
                     # Add map box with point dragger
                     box(width = NULL,
                         leafletOutput(outputId = "locationMap"),
                         htmlOutput(outputId = "text"),
                         absolutePanel(
                           top = 50, left = 70, draggable = TRUE, width = "20%",
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
                     )), # end map row
            # right - hand column
            fluidRow(width = 3,
                     box(width = NULL,
                         title = tagList(icon("plus"), strong("Series of Consumer Inputs")), 
                         status = "primary", collapsible = TRUE,
                         "Caught a fish off the coast of Southern California? Fill the required fields below to better understand the levels of contamination."
                     ),
                     # Prediction Box
                     box(width = NULL, title = "Prediction Result", status = "success", solidHeader = TRUE,
                         collapsible = TRUE,
                         verbatimTextOutput("prediction")
                     )  
            ) 
    ), # END what's in my catch tab item
    
    # about tabItem ----
    tabItem(tabName = "project",
            
            # title row ----
            fluidRow(width = 12,
                   
                   #title box ----
                   box(width = NULL,
                       title = tags$h2(strong("Improving Access to Fish Consumption Advisories and Maintaining Confidence in California's Healthy Seafood Products"))
                       
                   ), # END background info box 
                   
                   #background info box ----
                   box(width = NULL,
                       title = tagList(strong("Project Background")),
                       HTML("Dichlorodiphenyltrichloroethane (DDT) is an insecticide that is resistant to degradation and can cause increased risks of cancer, premature births, developmental abnormalities, and neurological diseases in humans and animals. A recent <a href='https://www.latimes.com/environment/story/2022-05-18/heres-what-we-know-about-the-legacy-of-ddt-dumping-near-catalina'>rediscovery</a> of a vast barrel field of DDT-laced sludge off the coast of southern California has captured the attention of the public and raised concerns regarding consumption of contaminated seafood. Alongside direct public health impacts, a decrease in seafood consumers poses a threat to the regional economy and recreational fishing communities. This project helps inform the public and give users the autonomy to understand the risk and make informed decisions on their seafood consumption. The interactive element of this application will allow users to access predicted concentrations of total DDT in seafood catch based on their location and the specific species of their catch."),
                       # tags$img(src = "dumpsite.png.jpeg", 
                       #          alt = "Map of fishing zones and the number of fish samples through time, by region (inset). Nearshore 708 polygons are derived from McLaughlin et al. (2021) and pink blocks are California Department of Fish and Game 256 km2 709 fishing blocks.",
                       #          style = "max-width: 90%; display: block; margin: 0 auto;")
                       
                   ) # END background info box
                   
            ), # END left-hand column 
            
    ), # END about tabItem
    
    # about tabItem ----
    tabItem(tabName = "user_manual",
            
            # column ----
            column(width = 12,
                   
                   #background info box ----
                   box(width = NULL,
                       title = tagList(strong("How to Use:")),
                       "Different sections in this area can detail how to use the applciation and how to apply the outputs to your life."
                   ), # END background info box 
                   
                   #section 1:  box ----
                   box(width = 4,
                       title = tagList(strong("Section 1:")),
                       "This can be the first section"
                   ), # END section 1: box
                   
                   #section 2:  box ----
                   box(width = 4,
                       title = tagList(strong("Section 2:")),
                       "This can be another section"
                   ), # END section 2: box
                   
                   #section 3:  box ----
                   box(width = 4,
                       title = tagList(strong("Section 3:")),
                       "This can be another section"
                   ) # END section 3: box
                   
            ), # END left-hand column 
            
    ), # END about tabItem
    
    # about tabItem ----
    tabItem(tabName = "authors",
            
            # left - hand column ----
            column(width = 12,
                   
                   #background info box ----
                   box(width = 3,
                       title = tagList(strong("Authors")),
                       p("This application was developed as part of a Masters in Environmental Data Science Capstone project as part of the Bren School, for Scripps and CalCOFI. "),
                       p("This project was completed by a group of graduate students at the Bren School of Environmental Science & Management, UC Santa Barbara. Team members include Hope Hahn, Luna Herschenfeld-Catalán, Benjamin Versteeg, and Kate Becker with guidance from our Faculty Advisor Bruce Kendall and Capstone Advisor Carmen Galaz-García.")
                       
                   ) # END author info box 
                  
            ) # END left-hand column 
            
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



