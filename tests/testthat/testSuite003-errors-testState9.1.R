context("Test Suite 3 (Errors) --> State 9.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 3 (Errors) --> State 9.1")
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

  test_that("can navigate to state 9.1", {
   navigateToState(remDr, "9.1")
  })

  test_that("Giving an invalid n1 throws correct error", {
    clear(remDr, pages$state9.1$selectors$n1Input)
    sendKeys(remDr, pages$state9.1$selectors$n1Input, "-1")
    clickGo(remDr)
    checkError(remDr, "n1 must be an integer greater than or equal to 1", "n12")
    clear(remDr, pages$state9.1$selectors$n1Input)
    sendKeys(remDr, pages$state9.1$selectors$n1Input, "1.5")
    clickGo(remDr)
    checkError(remDr, "n1 must be an integer greater than or equal to 1", "n12")
    clear(remDr, pages$state9.1$selectors$n1Input)
    clickGo(remDr)
    checkError(remDr, "n1 must be an integer greater than or equal to 1", "n12")
    # Reset for upcoming tests
    clear(remDr, pages$state9.1$selectors$n1Input)
    sendKeys(remDr, pages$state9.1$selectors$n1Input, "500")
  })

  test_that("Giving an invalid n2 throws correct error", {
    clear(remDr, pages$state9.1$selectors$n2Input)
    sendKeys(remDr, pages$state9.1$selectors$n2Input, "-1")
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n22")
    clear(remDr, pages$state9.1$selectors$n2Input)
    sendKeys(remDr, pages$state9.1$selectors$n2Input, "1.5")
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n22")
    clear(remDr, pages$state9.1$selectors$n2Input)
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n22")
    # Reset for upcoming tests
    clear(remDr, pages$state9.1$selectors$n2Input)
    sendKeys(remDr, pages$state9.1$selectors$n2Input, "100")
  })

  test_that("Giving an invalid thin throws correct error", {
    clear(remDr, pages$state9.1$selectors$thinInput)
    sendKeys(remDr, pages$state9.1$selectors$thinInput, "-1")
    clickGo(remDr)
    checkError(remDr, "thin must be an integer greater than or equal to 1", "thin")
    clear(remDr, pages$state9.1$selectors$thinInput)
    sendKeys(remDr, pages$state9.1$selectors$thinInput, "1.5")
    clickGo(remDr)
    checkError(remDr, "thin must be an integer greater than or equal to 1", "thin")
    clear(remDr, pages$state9.1$selectors$thinInput)
    clickGo(remDr)
    checkError(remDr, "thin must be an integer greater than or equal to 1", "thin")
    # Reset for upcoming tests
    clear(remDr, pages$state9.1$selectors$thinInput)
    sendKeys(remDr, pages$state9.1$selectors$thinInput, "10")
  })

  test_that("Giving an invalid burnin throws correct error", {
    clear(remDr, pages$state9.1$selectors$burninInput)
    sendKeys(remDr, pages$state9.1$selectors$burninInput, "-1")
    clickGo(remDr)
    checkError(remDr, "burnin must be a non-negative integer", "burnin")
    clear(remDr, pages$state9.1$selectors$burninInput)
    sendKeys(remDr, pages$state9.1$selectors$burninInput, "1.5")
    clickGo(remDr)
    checkError(remDr, "burnin must be a non-negative integer", "burnin")
    clear(remDr, pages$state9.1$selectors$burninInput)
    clickGo(remDr)
    checkError(remDr, "burnin must be a non-negative integer", "burnin")
    # Reset for upcoming tests
    clear(remDr, pages$state9.1$selectors$burninInput)
    sendKeys(remDr, pages$state9.1$selectors$burninInput, "3000")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
