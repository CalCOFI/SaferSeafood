library(shinytest2)
library(shiny)

# This block defines a test case titled "Functionality of UI interactions"
test_that("Functionality of UI interactions", {
  # Initializes a new ShinyDriver instance, pointing to the directory of the Shiny app
  app <- AppDriver$new("./")
  # Ensures that the Shiny app is stopped when the test ends
  on.exit(app$stop())
  
  # Test selecting a species from a dropdown menu
  # This sets the 'species_dropdown' input to "White Croaker"
  app$set_inputs(species_dropdown = "White Croaker")
  # Verifies that the dropdown correctly reports "White Croaker" as its value
  expect_equal(app$get_value("species_dropdown"), "White Croaker")
  
  # Test the functionality of a predict button
  # This simulates a button click by setting the 'predict_button' input to "click"
  app$set_inputs(predict_button = "click")
  # Checks if the plot output element is present in the UI after clicking the button
  expect_true(app$is_present("output_plot"))
  
  # Test updating coordinates by simulating dragging markers on a map
  # Executes JavaScript to simulate dragging a map marker to a new location
  app$execute_script("map.dragMarkerTo({lat: 35.6895, lon: 139.6917})")
  # Verifies that the latitude input field updates to the new latitude (35.6895)
  expect_equal(app$get_value("latitude_input"), 35.6895)
  # Verifies that the longitude input field updates to the new longitude (139.6917)
  expect_equal(app$get_value("longitude_input"), 139.6917)
})






