context("Test Suite 2 (States) --> State 5.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 5.1")
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

  test_that("can navigate to state 5.1", {
   navigateToState(remDr, "5.1")
  })

  test_that("patient exposure data inputs are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state5.1$selectors$exposureDataLabel))
    expect_equal(getText(remDr, pages$state5.1$selectors$exposureDataLabel),
       paste("Do you want to use serial interval data from individual patients, or use a",
             "distributional estimate of the serial interval?"))
    expect_true(isDisplayed(remDr, pages$state5.1$selectors$exposureDataNoLabel))
    expect_equal(getText(remDr, pages$state5.1$selectors$exposureDataNoLabel), "Distributional Estimate")
    expect_true(isDisplayed(remDr, pages$state5.1$selectors$exposureDataNoInput))
    expect_true(isDisplayed(remDr, pages$state5.1$selectors$exposureDataYesLabel))
    expect_equal(getText(remDr, pages$state5.1$selectors$exposureDataYesLabel), "Individual Patient Data")
    expect_true(isDisplayed(remDr, pages$state5.1$selectors$exposureDataYesInput))
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
