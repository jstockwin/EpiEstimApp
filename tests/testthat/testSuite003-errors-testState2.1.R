context("Test Suite 3 (Errors) --> State 2.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 3 (Errors) --> State 2.1")
rD <- drivers$rDr
remDr <- drivers$remDr

openRemDriver(remDr)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr)
  })

  test_that("can navigate to state 2.1", {
   navigateToState(remDr, "2.1")
  })

  test_that("pressing next without uploading a file throws correct error", {
    clickNext(remDr)
    Sys.sleep(1)
    checkError(remDr, "Please upload a file", "incidence_data")
  })

  test_that("uploading a non-csv file throws correct error", {
    if (getAttribute(remDr, pages$state2.1$selectors$incidence_data_upload_input, "value") == "") {
      setAttribute(remDr, pages$state2.1$selectors$incidence_data_upload_input, "style", "display: block;")
    }
    path <- getFilePath(remDr, "utils.R")
    #path <- getFilePath(remDr, "datasets/IncidenceData/H1N1Pennsylvania2009.csv")
    sendKeys(remDr, pages$state2.1$selectors$incidence_data_upload_input, path)
    clickNext(remDr)
    Sys.sleep(1)
    checkError(remDr, "The uploaded file must be a .csv file", "incidence_data")
    # Reset for upcoming tests:
    connectToApp(remDr)
    waitForAppReady(remDr)
    navigateToState(remDr, "2.1")
    if (getAttribute(remDr, pages$state2.1$selectors$incidence_data_upload_input, "value") == "") {
      setAttribute(remDr, pages$state2.1$selectors$incidence_data_upload_input, "style", "display: block;")
    }
    path <- getFilePath(remDr, "datasets/IncidenceData/H1N1Pennsylvania2009.csv")
    sendKeys(remDr, pages$state2.1$selectors$incidence_data_upload_input, path)
  })

  test_that("Giving an invalid prior mean throws correct error", {
    clear(remDr, pages$state2.1$selectors$mean_prior_input)
    sendKeys(remDr, pages$state2.1$selectors$mean_prior_input, "-1")
    clickNext(remDr)
    checkError(remDr, "Prior mean must be non-negative", "uploaded_mean_prior")
    # Reset for upcoming tests
    clear(remDr, pages$state2.1$selectors$mean_prior_input)
    sendKeys(remDr, pages$state2.1$selectors$mean_prior_input, "5")
    clickPrev(remDr)
    waitForStateDisplayed(remDr, "1.1")
    clickNext(remDr)
    waitForStateDisplayed(remDr, "2.1")
  })

  test_that("Giving an invalid prior sd throws correct error", {
    clear(remDr, pages$state2.1$selectors$std_prior_input)
    sendKeys(remDr, pages$state2.1$selectors$std_prior_input, "0")
    clickNext(remDr)
    checkError(remDr, "Prior standard deviation must be positive", "uploaded_std_prior")
    # Reset for upcoming tests
    clear(remDr, pages$state2.1$selectors$std_prior_input)
    sendKeys(remDr, pages$state2.1$selectors$std_prior_input, "5")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
