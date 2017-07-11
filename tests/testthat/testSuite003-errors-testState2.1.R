context("Test Suite 2 (States) --> State 2.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 2.1")
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
    checkError(remDr, "Please upload a file", "incidenceData")
  })

  test_that("uploading a non-csv file throws correct error", {
    if (getAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "value") == "") {
      setAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "style", "display: block;")
    }
    path <- getFilePath(remDr, "utils.R")
    #path <- getFilePath(remDr, "datasets/IncidenceData/FluPennsylvania2009.csv")
    sendKeys(remDr, pages$state2.1$selectors$incidenceDataUploadInput, path)
    clickNext(remDr)
    Sys.sleep(1)
    checkError(remDr, "The uploaded file must be a .csv file", "incidenceData")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
