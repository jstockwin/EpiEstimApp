context("Test Suite 2 (States) --> State 1.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 1.1")
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

  test_that("only state 1.1 is displayed", {
    checkDisplayedState(remDr, "1.1")
  })

  test_that("incidence data type buttons are displaying correctly", {
    # Check the div is displaying
    expect_true(isDisplayed(remDr, pages$state1.1$selectors$incidenceDataType))
    # Check the label is correct
    expect_true(isDisplayed(remDr, pages$state1.1$selectors$incidenceDataTypeLabel))
    expect_equal(getText(remDr, pages$state1.1$selectors$incidenceDataTypeLabel),
                 "Do you want to use pre-loaded incidence time series data or upload your own?")
    # Check the first radio input button (pre-loaded option)
    expect_equal(getAttribute(remDr, pages$state1.1$selectors$preloadedDataButton, "value"), "preloaded")
    expect_equal(getText(remDr, pages$state1.1$selectors$preloadedDataLabel), "Pre-loaded")
    # Check the second radio input button (own data option)
    expect_equal(getAttribute(remDr, pages$state1.1$selectors$ownDataButton, "value"), "own")
    expect_equal(getText(remDr, pages$state1.1$selectors$ownDataLabel), "Own data")
  })

  test_that("relevant control buttons are displayed", {
    expect_false(isDisplayed(remDr, pages$common$selectors$stopButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$prevButton))
    expect_false(isEnabled(remDr, pages$common$selectors$prevButton))
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
