context("Test Suite 3 (Errors) --> State 8.2")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 3 (Errors) --> State 8.2")
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

  test_that("can navigate to state 8.2", {
   navigateToState(remDr, "8.2")
  })

  test_that("pressing next without uploading a file throws correct error", {
    clickNext(remDr)
    Sys.sleep(1)
    checkError(remDr, "Please upload a file", "SIData")
  })

  test_that("uploading a non-csv file throws correct error", {
    if (getAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "value") == "") {
      setAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "style", "display: block;")
    }
    path <- getFilePath(remDr, "utils.R")
    #path <- getFilePath(remDr, "datasets/IncidenceData/FluPennsylvania2009.csv")
    sendKeys(remDr, pages$state8.2$selectors$SIDataUploadInput, path)
    clickNext(remDr)
    Sys.sleep(1)
    checkError(remDr, "The uploaded file must be a .csv file", "SIData")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
