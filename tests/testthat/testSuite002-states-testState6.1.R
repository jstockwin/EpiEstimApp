context("Test Suite 2 (States) --> State 6.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 6.1")
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

  test_that("can navigate to state 6.1", {
    navigateToState(remDr, "6.1")
  })

  test_that("only state 6.1 is displayed", {
    checkDisplayedState(remDr, "6.1")
  })

  test_that("SIDataType input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$SIDataType))
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$SIDataTypeLabel))
    expect_equal(getText(remDr, pages$state6.1$selectors$SIDataTypeLabel),
                 paste("Would you like to use an external file containing",
                       "the exposure data, or a pre-loaded dataset?"))
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$SIDataTypePreloadedLabel))
    expect_equal(getText(remDr, pages$state6.1$selectors$SIDataTypePreloadedLabel), "Pre-loaded")
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$SIDataTypePreloadedButton))
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$SIDataTypeOwnLabel))
    expect_equal(getText(remDr, pages$state6.1$selectors$SIDataTypeOwnLabel), "Own data")
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$SIDataTypeOwnButton))
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
