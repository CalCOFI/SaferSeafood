

#........................dashboardSidebar........................

#..........................dashboardBody.........................
body <- navbarPage(
  
  "SaferSeafood", # title
  
  header = includeCSS("www/styles.css"),
  #theme = shinythemes::shinytheme('darkly'),
  
  tabPanel("What's In My Catch?", 
           
           column(width = 6,
                    # Add map box with point dragger
                    box(width = NULL,
                        leafletOutput(outputId = "locationMap"),
                        htmlOutput(outputId = "text"),
                    )
           ),
           column(width = 3,
                  box(width = 12,
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
                      ))
                  ),
           column(width = 3,
                  box(width = 12,
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
                  box(width = 12, 
                      title = "Health Advisories", status = "success", solidHeader = TRUE,
                      collapsible = TRUE,
                      imageOutput(outputId = "advisory_image"))
           ) # end of whats in my catch panel
           
  ),
  tabPanel("User Manual", 
           
           
  ),
  tabPanel("About", 
           
           
  ),
  navbarMenu("Resources", 
             tabPanel("Fish Identification", "four-a"),
             tabPanel("Collaborators", "four-b")
  )
)



#..................combine all in dashboardPage..................

navbarPage(body)



