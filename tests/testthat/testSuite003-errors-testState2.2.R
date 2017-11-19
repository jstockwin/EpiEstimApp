context("Test Suite 3 (Errors) --> State 2.2")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 3 (Errors) --> State 2.2")
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

  test_that("can navigate to state 2.2", {
   navigateToState(remDr, "2.2")
  })

  test_that("Giving an invalid prior mean throws correct error", {
    clear(remDr, pages$state2.2$selectors$mea_priorInput)
    sendKeys(remDr, pages$state2.2$selectors$mea_priorInput, "-1")
    clickNext(remDr)
    checkError(remDr, "Prior mean must be non-negative", "incidenceMea_prior")
    # Reset for upcoming tests
    clear(remDr, pages$state2.2$selectors$mea_priorInput)
    sendKeys(remDr, pages$state2.2$selectors$mea_priorInput, "5")
  })

  test_that("Giving an invalid prior sd throws correct error", {
    clear(remDr, pages$state2.2$selectors$stdPriorInput)
    sendKeys(remDr, pages$state2.2$selectors$stdPriorInput, "0")
    clickNext(remDr)
    checkError(remDr, "Prior standard deviation must be positive", "incidenceStdPrior")
    # Reset for upcoming tests
    clear(remDr, pages$state2.2$selectors$stdPriorInput)
    sendKeys(remDr, pages$state2.2$selectors$stdPriorInput, "5")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
