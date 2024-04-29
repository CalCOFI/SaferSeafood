library(shinytest2)
library(shiny)



test_that("Bayesian model performance", {
  start_time <- Sys.time()
  model <- brm.diet.habitat.year.fam.clean
  end_time <- Sys.time()
  expect_true(difftime(end_time, start_time, units = "secs") < 30, "Should run in less than 30 seconds")
})




# # Test doesn't work because "input" integrated with the Shiny UI and environment
# test_that("predict_DDT returns expected values", {
#   species <- "White Croaker"
#   latitude <- 34.2749
#   longitude <- -119.292
#   prediction <- predict_DDT(species, latitude, longitude)
# 
#   # Assuming predict_DDT returns a numeric prediction
#   expect_is(prediction, "numeric")
#   expect_true(prediction > 0) # assuming prediction should always be positive
# })








