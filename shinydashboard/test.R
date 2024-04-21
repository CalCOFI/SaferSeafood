shinyUI(fluidPage(
  
  titlePanel("Using Geolocation"),
  
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
              '),
  
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
  
)
)



# whats_in_my_catch tabItem
tabItem(tabName = "whats_in_my_catch",
        
        # fluidRow ----
        fluidPage(
          
          titlePanel("Using Geolocation"),
          
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
              '),
          
          # Left-hand column content
          fluidRow(width = NULL,
                   
                   # add map box
                   box(width = NULL,
                       
                       leafletOutput(outputId = "location_output")
                       
                       
                   ) # END of map box
          )
          
        )
        
        
        
        # #other chemical advisory info box
        # box(width = NULL,
        #     title = tagList(strong("Mercury and PCB Consumption Advice")),
        #     "",
        #     tags$img(src = "fish.png", 
        #              alt = "For more information regarding OEHHA fish advisory program, visit https://oehha.ca.gov/fish/advisories.",
        #              style = "max-width: 90%; display: block; margin: 0 auto;")
        # ), # END chemical advisory info box 
        # 
        # box(width = NULL,
        #     title = tagList(strong("Following Guidelines")),
        #     "",
        #     tags$img(src = "advising-fish-guide.png",
        #              alt = "Guidelines",
        #              style = "max-width: 90%; display: block; margin: 0 auto;")
        # ) # end GUIDELINES box 
        
        
), #end map row