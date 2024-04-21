library(shiny)
library(shinydashboard)

jscode <- "
  window.open('','_parent','');
  window.close();
"

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    actionButton("close", "Close app"),
    tags$script(
      "Shiny.addCustomMessageHandler('closeWindow', function(data) {
       eval(data.message)
      });"
    )
  ) 
)

server = function(input, output, session) {
  observeEvent(input$close, {
    
    session$sendCustomMessage(type = "closeWindow", list(message = jscode))
    
  })
}

runApp(list(ui = ui, server = server), launch.browser =T)