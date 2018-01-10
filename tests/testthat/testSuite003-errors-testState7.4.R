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

  test_that("Giving an invalid mean_si throws correct error", {
    clear(remDr, pages$state7.4$selectors$mean_siInput)
    sendKeys(remDr, pages$state7.4$selectors$mean_siInput, "-1")
    clickGo(remDr)
    checkError(remDr, "mean_si must be greater than 1", "mean_si2")
    clear(remDr, pages$state7.4$selectors$mean_siInput)
    sendKeys(remDr, pages$state7.4$selectors$mean_siInput, "1")
    clickGo(remDr)
    checkError(remDr, "mean_si must be greater than 1", "mean_si2")
    # Reset for upcoming tests
    clear(remDr, pages$state7.4$selectors$mean_siInput)
    sendKeys(remDr, pages$state7.4$selectors$mean_siInput, "2")
    clickPrev(remDr)
    waitForStateDisplayed(remDr, "6.2")
    clickNext(remDr)
    waitForStateDisplayed(remDr, "7.4")
  })

  test_that("Giving an invalid std_si throws correct error", {
    clear(remDr, pages$state7.4$selectors$std_siInput)
    sendKeys(remDr, pages$state7.4$selectors$std_siInput, "-1")
    clickGo(remDr)
    checkError(remDr, "std_si must be greater than 0", "std_si2")
    clear(remDr, pages$state7.4$selectors$std_siInput)
    sendKeys(remDr, pages$state7.4$selectors$std_siInput, "0")
    clickGo(remDr)
    checkError(remDr, "std_si must be greater than 0", "std_si2")
    # Reset for upcoming tests
    clear(remDr, pages$state7.4$selectors$std_siInput)
    sendKeys(remDr, pages$state7.4$selectors$std_siInput, "1")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
