library(shinytest2)
library(shinytest)
library(shiny)
library(testthat)

# test_that("Functionality of UI interactions", {
#   app <- ShinyDriver$new("./")
#   on.exit(app$stop())
#   
#   # Test selecting a species
#   app$set_inputs(species_dropdown = "White Croaker")
#   expect_equal(app$get_value("species_dropdown"), "White Croaker")
#   
#   # Test the predict button functionality
#   app$set_inputs(predict_button = "click")
#   expect_true(app$is_present("output_plot"))
#   
#   # Test updating coordinates by dragging markers
#   # This assumes you can simulate dragging in your testing environment
#   app$execute_script("map.dragMarkerTo({lat: 35.6895, lon: 139.6917})")
#   expect_equal(app$get_value("latitude_input"), 35.6895)
#   expect_equal(app$get_value("longitude_input"), 139.6917)
# })





