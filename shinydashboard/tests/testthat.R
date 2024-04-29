
library(shinytest2)
library(shiny)

# Run tests only if the SHINY_TEST environment variable is set to "true"
if (Sys.getenv("SHINY_TEST") == "true") {
  message("Running Shiny tests...")
  results <- test_dir(
    path = "./testthat",
    env = shiny::loadSupport(),  # Ensure this loads the necessary Shiny app components
    reporter = c("summary", "fail")  # Using 'summary' for a more comprehensive report
  )
  
  # Optionally, handle results programmatically here
  if (!all_successful(results)) {
    stop("Some tests failed. Check the logs for details.", call. = FALSE)
  }
} else {
  message("SHINY_TEST is not set to 'true'; skipping tests.")
}
