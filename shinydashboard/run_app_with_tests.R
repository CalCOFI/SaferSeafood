# Set the environment variable to enable testing mode
Sys.setenv(SHINY_TEST = "true")

# Load necessary libraries
library(shiny)

# Set the working directory to the directory containing the Shiny application
 here::here("shinydashboard")

# Source the global and server scripts
source("global.R")
source("server.R")

# Runs all the tests in the testthat folder
testthat::test_dir("tests/testthat")

# Run the app 
shiny::runApp()

# Reset the environment variable or handle other cleanup tasks
Sys.unsetenv("SHINY_TEST")

# You can run this script directly by using:
# source("run_app_with_tests.R")
# in the console

