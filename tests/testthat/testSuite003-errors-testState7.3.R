context("Test Suite 3 (Errors) --> State 7.3")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 3 (Errors) --> State 7.3")
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

  test_that("can navigate to state 7.3", {
   navigateToState(remDr, "7.3")
  })

  test_that("Giving an invalid n1 throws correct error", {
    clear(remDr, pages$state7.3$selectors$n1_input)
    sendKeys(remDr, pages$state7.3$selectors$n1_input, "-1")
    clickGo(remDr)
    checkError(remDr, "n1 must be an integer greater than or equal to 1", "n1")
    clear(remDr, pages$state7.3$selectors$n1_input)
    sendKeys(remDr, pages$state7.3$selectors$n1_input, "1.5")
    clickGo(remDr)
    checkError(remDr, "n1 must be an integer greater than or equal to 1", "n1")
    clear(remDr, pages$state7.3$selectors$n1_input)
    clickGo(remDr)
    checkError(remDr, "n1 must be an integer greater than or equal to 1", "n1")
    # Reset for upcoming tests
    clear(remDr, pages$state7.3$selectors$n1_input)
    sendKeys(remDr, pages$state7.3$selectors$n1_input, "50")
    clickPrev(remDr)
    waitForStateDisplayed(remDr, "6.2")
    clickNext(remDr)
    waitForStateDisplayed(remDr, "7.3")
  })

  test_that("Giving an invalid n2 throws correct error", {
    clear(remDr, pages$state7.3$selectors$n2_input)
    sendKeys(remDr, pages$state7.3$selectors$n2_input, "-1")
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n2")
    clear(remDr, pages$state7.3$selectors$n2_input)
    sendKeys(remDr, pages$state7.3$selectors$n2_input, "1.5")
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n2")
    clear(remDr, pages$state7.3$selectors$n2_input)
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n2")
    # Reset for upcoming tests
    clear(remDr, pages$state7.3$selectors$n2_input)
    sendKeys(remDr, pages$state7.3$selectors$n2_input, "50")
  })

  test_that("Giving an invalid mean_si throws correct error", {
    clear(remDr, pages$state7.3$selectors$mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$mean_si_input, "-1")
    clickGo(remDr)
    checkError(remDr, "mean_si must be greater than or equal to 1", "mean_si")
    clear(remDr, pages$state7.3$selectors$mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$mean_si_input, "0.99")
    clickGo(remDr)
    checkError(remDr, "mean_si must be greater than or equal to 1", "mean_si")
    clear(remDr, pages$state7.3$selectors$mean_si_input)
    clickGo(remDr)
    checkError(remDr, "mean_si must be greater than or equal to 1", "mean_si")
    # Reset
    clear(remDr, pages$state7.3$selectors$mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$mean_si_input, "2")
  })

  test_that("Giving an invalid min_mean_si throws correct error", {
    clear(remDr, pages$state7.3$selectors$min_mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$min_mean_si_input, "-1")
    clickGo(remDr)
    checkError(remDr, "min_mean_si must be greater than or equal to 1", "min_mean_si")
    clear(remDr, pages$state7.3$selectors$min_mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$min_mean_si_input, "0.99")
    clickGo(remDr)
    checkError(remDr, "min_mean_si must be greater than or equal to 1", "min_mean_si")
    clear(remDr, pages$state7.3$selectors$min_mean_si_input)
    clickGo(remDr)
    checkError(remDr, "min_mean_si must be greater than or equal to 1", "min_mean_si")
    clear(remDr, pages$state7.3$selectors$min_mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$min_mean_si_input, "2.5")
    clickGo(remDr)
    checkError(remDr, "min_mean_si must be less than mean_si", "mean_si")
    checkError(remDr, "min_mean_si must be less than mean_si", "min_mean_si")
    # Reset
    clear(remDr, pages$state7.3$selectors$min_mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$min_mean_si_input, "1")
  })

  test_that("Giving an invalid max_mean_si throws correct error", {
    clear(remDr, pages$state7.3$selectors$max_mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$max_mean_si_input, "-1")
    clickGo(remDr)
    checkError(remDr, "max_mean_si must be greater than or equal to 1", "max_mean_si")
    clear(remDr, pages$state7.3$selectors$max_mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$max_mean_si_input, "0.99")
    clickGo(remDr)
    checkError(remDr, "max_mean_si must be greater than or equal to 1", "max_mean_si")
    clear(remDr, pages$state7.3$selectors$max_mean_si_input)
    clickGo(remDr)
    checkError(remDr, "max_mean_si must be greater than or equal to 1", "max_mean_si")
    clear(remDr, pages$state7.3$selectors$max_mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$max_mean_si_input, "1.5")
    clickGo(remDr)
    checkError(remDr, "max_mean_si must be greater than mean_si", "max_mean_si")
    checkError(remDr, "max_mean_si must be greater than mean_si", "mean_si")
    # Reset
    clear(remDr, pages$state7.3$selectors$max_mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$max_mean_si_input, "3")
  })

  test_that("Giving an invalid std_mean_si throws correct error", {
    clear(remDr, pages$state7.3$selectors$std_mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$std_mean_si_input, "-1")
    clickGo(remDr)
    checkError(remDr, "std_mean_si must be greater than 0", "std_mean_si")
    clear(remDr, pages$state7.3$selectors$std_mean_si_input)
    clickGo(remDr)
    checkError(remDr, "std_mean_si must be greater than 0", "std_mean_si")
    # Reset
    clear(remDr, pages$state7.3$selectors$std_mean_si_input)
    sendKeys(remDr, pages$state7.3$selectors$std_mean_si_input, "1")
  })

  test_that("Giving an invalid std_si throws correct error", {
    clear(remDr, pages$state7.3$selectors$std_si_input)
    sendKeys(remDr, pages$state7.3$selectors$std_si_input, "-1")
    clickGo(remDr)
    checkError(remDr, "std_si must be greater than 0", "std_si")
    clear(remDr, pages$state7.3$selectors$std_si_input)
    clickGo(remDr)
    checkError(remDr, "std_si must be greater than 0", "std_si")
    # Reset
    clear(remDr, pages$state7.3$selectors$std_si_input)
    sendKeys(remDr, pages$state7.3$selectors$std_si_input, "2")
  })

  test_that("Giving an invalid min_std_si throws correct error", {
    clear(remDr, pages$state7.3$selectors$min_std_si_input)
    sendKeys(remDr, pages$state7.3$selectors$min_std_si_input, "-1")
    clickGo(remDr)
    checkError(remDr, "min_std_si must be greater than 0", "min_std_si")
    clear(remDr, pages$state7.3$selectors$min_std_si_input)
    clickGo(remDr)
    checkError(remDr, "min_std_si must be greater than 0", "min_std_si")
    clear(remDr, pages$state7.3$selectors$min_std_si_input)
    sendKeys(remDr, pages$state7.3$selectors$min_std_si_input, "2.5")
    clickGo(remDr)
    checkError(remDr, "min_std_si must be less than std_si", "std_si")
    checkError(remDr, "min_std_si must be less than std_si", "min_std_si")
    # Reset
    clear(remDr, pages$state7.3$selectors$min_std_si_input)
    sendKeys(remDr, pages$state7.3$selectors$min_std_si_input, "2")
  })

  test_that("Giving an invalid max_std_si throws correct error", {
    clear(remDr, pages$state7.3$selectors$max_std_si_input)
    sendKeys(remDr, pages$state7.3$selectors$max_std_si_input, "-1")
    clickGo(remDr)
    checkError(remDr, "max_std_si must be greater than 0", "max_std_si")
    clear(remDr, pages$state7.3$selectors$max_std_si_input)
    clickGo(remDr)
    checkError(remDr, "max_std_si must be greater than 0", "max_std_si")
    clear(remDr, pages$state7.3$selectors$max_std_si_input)
    sendKeys(remDr, pages$state7.3$selectors$max_std_si_input, "1.5")
    clickGo(remDr)
    checkError(remDr, "max_std_si must be greater than std_si", "max_std_si")
    checkError(remDr, "max_std_si must be greater than std_si", "std_si")
    # Reset
    clear(remDr, pages$state7.3$selectors$max_std_si_input)
    sendKeys(remDr, pages$state7.3$selectors$max_std_si_input, "3")
  })

  test_that("Giving an invalid std_std_si throws correct error", {
    clear(remDr, pages$state7.3$selectors$std_std_si_input)
    sendKeys(remDr, pages$state7.3$selectors$std_std_si_input, "-1")
    clickGo(remDr)
    checkError(remDr, "std_std_si must be greater than 0", "std_std_si")
    clear(remDr, pages$state7.3$selectors$std_std_si_input)
    clickGo(remDr)
    checkError(remDr, "std_std_si must be greater than 0", "std_std_si")
    # Reset
    clear(remDr, pages$state7.3$selectors$std_std_si_input)
    sendKeys(remDr, pages$state7.3$selectors$std_std_si_input, "1")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
