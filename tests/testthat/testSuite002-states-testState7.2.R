context("Test Suite 2 (States) --> State 7.2")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 7.2")
rD <- drivers$rDr
remDr <- drivers$remDr

remDr$open(silent=TRUE)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr)
  })

  test_that("can navigate to state 7.2", {
    navigateToState(remDr, "7.2")
  })

  test_that("only state 7.2 is displayed", {
    checkDisplayedState(remDr, "7.2")
  })

  test_that("SIFrom input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$SIFrom))
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$SIFromLabel))
    expect_equal(getText(remDr, pages$state7.2$selectors$SIFromLabel),
                       "Do you have raw exposure data or a SI posterior sample to upload?")
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$SIFromRawLabel))
    expect_equal(getText(remDr, pages$state7.2$selectors$SIFromRawLabel), "Raw exposure data")
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$SIFromRawButton))
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$SIFromSampleLabel))
    expect_equal(getText(remDr, pages$state7.2$selectors$SIFromSampleLabel), "SI posterior sample")
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$SIFromSampleButton))
  })

  test_that("relevant control buttons are displayed", {
    expect_false(isDisplayed(remDr, pages$common$selectors$stopButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$prevButton))
    expect_true(isEnabled(remDr, pages$common$selectors$prevButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$nextButton))
    expect_true(isEnabled(remDr, pages$common$selectors$nextButton))
    expect_false(isDisplayed(remDr, pages$common$selectors$goButton))
  })

  test_that("no errors are displaying", {
    expect_true(isDisplayed(remDr, pages$common$selectors$errorMessage))
    expect_equal(getText(remDr, pages$common$selectors$errorMessage), "")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
