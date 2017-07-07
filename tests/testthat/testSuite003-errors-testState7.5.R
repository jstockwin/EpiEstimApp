context("Test Suite 2 (States) --> State 7.5")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 7.5")
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
    checkError(remDr, "Please upload a file", "SIDistrData")
  })

  test_that("uploading a non-csv file throws correct error", {
    if (getAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "value") == "") {
      setAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "style", "display: block;")
    }
    path <- getFilePath(remDr, "utils.R")
    #path <- getFilePath(remDr, "datasets/IncidenceData/FluPennsylvania2009.csv")
    sendKeys(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, path)
    Sys.sleep(1)
    clickGo(remDr)
    Sys.sleep(1)
    checkError(remDr, "The uploaded file must be a .csv file", "SIDistrData")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
