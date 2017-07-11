context("Test Suite 3 (Errors) --> State 8.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 3 (Errors) --> State 8.1")
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

  test_that("can navigate to state 8.1", {
   navigateToState(remDr, "8.1")
  })

  test_that("Giving an invalid n2 throws correct error", {
    clear(remDr, pages$state8.1$selectors$n2Input)
    sendKeys(remDr, pages$state8.1$selectors$n2Input, "-1")
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n24")
    clear(remDr, pages$state8.1$selectors$n2Input)
    sendKeys(remDr, pages$state8.1$selectors$n2Input, "1.5")
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n24")
    # Reset for upcoming tests
    clear(remDr, pages$state8.1$selectors$n2Input)
    sendKeys(remDr, pages$state8.1$selectors$n2Input, "100")
  })

},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
