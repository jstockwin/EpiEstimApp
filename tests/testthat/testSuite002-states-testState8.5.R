context("Test Suite 2 (States) --> State 8.5")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 8.5")
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

  test_that("can navigate to state 8.5", {
    navigateToState(remDr, "8.5")
  })

  test_that("only state 8.5 is displayed", {
    checkDisplayedState(remDr, "8.5")
  })

  test_that("SIDistrDataType input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state8.5$selectors$SIDistrDataType))
    expect_true(isDisplayed(remDr, pages$state8.5$selectors$SIDistrDataTypeLabel))
    expect_equal(getText(remDr, pages$state8.5$selectors$SIDistrDataTypeLabel),
                 paste("Would you like to use an external file containing",
                       "the SI distribution, or a pre-loaded SI distribution?"))
    expect_true(isDisplayed(remDr, pages$state8.5$selectors$SIDistrDataTypePreloadedLabel))
    expect_equal(getText(remDr, pages$state8.5$selectors$SIDistrDataTypePreloadedLabel), "Pre-loaded")
    expect_true(isDisplayed(remDr, pages$state8.5$selectors$SIDistrDataTypePreloadedButton))
    expect_true(isDisplayed(remDr, pages$state8.5$selectors$SIDistrDataTypeOwnLabel))
    expect_equal(getText(remDr, pages$state8.5$selectors$SIDistrDataTypeOwnLabel), "External file")
    expect_true(isDisplayed(remDr, pages$state8.5$selectors$SIDistrDataTypeOwnButton))
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
