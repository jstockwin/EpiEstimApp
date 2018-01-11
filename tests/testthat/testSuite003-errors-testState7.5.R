context("Test Suite 3 (Errors) --> State 7.5")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 3 (Errors) --> State 7.5")
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

  test_that("can navigate to state 7.5", {
   navigateToState(remDr, "7.5")
  })

  test_that("pressing go without uploading a file throws correct error", {
    clickGo(remDr)
    Sys.sleep(1)
    checkError(remDr, "Please upload a file", "si_distr_data")
  })

  test_that("uploading a non-csv file throws correct error", {
    if (getAttribute(remDr, pages$state7.5$selectors$si_distr_data_upload_input, "value") == "") {
      setAttribute(remDr, pages$state7.5$selectors$si_distr_data_upload_input, "style", "display: block;")
    }
    path <- getFilePath(remDr, "utils.R")
    #path <- getFilePath(remDr, "datasets/IncidenceData/H1N1Pennsylvania2009.csv")
    sendKeys(remDr, pages$state7.5$selectors$si_distr_data_upload_input, path)
    clickGo(remDr)
    Sys.sleep(1)
    checkError(remDr, "The uploaded file must be a .csv file", "si_distr_data")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
