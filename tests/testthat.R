library(testthat)
devtools::build("/app")
test_check("EpiEstimApp")
