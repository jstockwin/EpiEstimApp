context("Test Suite 3 (Errors) --> State 7.4")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 3 (Errors) --> State 7.4")
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

  test_that("can navigate to state 7.4", {
   navigateToState(remDr, "7.4")
  })

  test_that("Giving an invalid Mean.SI throws correct error", {
    clear(remDr, pages$state7.4$selectors$Mean.SIInput)
    sendKeys(remDr, pages$state7.4$selectors$Mean.SIInput, "-1")
    clickGo(remDr)
    checkError(remDr, "Mean.SI must be greater than 1", "Mean.SI2")
    clear(remDr, pages$state7.4$selectors$Mean.SIInput)
    sendKeys(remDr, pages$state7.4$selectors$Mean.SIInput, "1")
    clickGo(remDr)
    checkError(remDr, "Mean.SI must be greater than 1", "Mean.SI2")
    # Reset for upcoming tests
    clear(remDr, pages$state7.4$selectors$Mean.SIInput)
    sendKeys(remDr, pages$state7.4$selectors$Mean.SIInput, "2")
  })

  test_that("Giving an invalid Std.SI throws correct error", {
    clear(remDr, pages$state7.4$selectors$Std.SIInput)
    sendKeys(remDr, pages$state7.4$selectors$Std.SIInput, "-1")
    clickGo(remDr)
    checkError(remDr, "Std.SI must be greater than 0", "Std.SI2")
    clear(remDr, pages$state7.4$selectors$Std.SIInput)
    sendKeys(remDr, pages$state7.4$selectors$Std.SIInput, "0")
    clickGo(remDr)
    checkError(remDr, "Std.SI must be greater than 0", "Std.SI2")
    # Reset for upcoming tests
    clear(remDr, pages$state7.4$selectors$Std.SIInput)
    sendKeys(remDr, pages$state7.4$selectors$Std.SIInput, "1")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
