context("Test Suite 2 (States) --> State 7.6")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 7.6")
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

  test_that("can navigate to state 7.6", {
    navigateToState(remDr, "7.6")
  })

  test_that("only state 7.6 is displayed", {
    checkDisplayedState(remDr, "7.6")
  })

  test_that("Dataset input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$datasetLabel))
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$datasetOption1Label))
    expect_equal(getText(remDr, pages$state7.6$selectors$datasetOption1Label),
                 "H1N1Maryland1918")
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$datasetOption1Input))
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$datasetOption2Label))
    expect_equal(getText(remDr, pages$state7.6$selectors$datasetOption2Label),
                 "H1N1Pennsylvania2009")
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$datasetOption2Input))
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$datasetOption3Label))
    expect_equal(getText(remDr, pages$state7.6$selectors$datasetOption3Label),
                 "MeaslesGermany1861")
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$datasetOption3Input))
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$datasetOption4Label))
    expect_equal(getText(remDr, pages$state7.6$selectors$datasetOption4Label),
                 "SARSHongKong2003")
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$datasetOption4Input))
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$datasetOption5Label))
    expect_equal(getText(remDr, pages$state7.6$selectors$datasetOption5Label),
                 "SmallpoxKosovo1972")
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$datasetOption5Input))
  })

  test_that("relevant control buttons are displayed", {
    expect_false(isDisplayed(remDr, pages$common$selectors$stopButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$prevButton))
    expect_true(isEnabled(remDr, pages$common$selectors$prevButton))
    expect_false(isDisplayed(remDr, pages$common$selectors$nextButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$goButton))
    expect_true(isEnabled(remDr, pages$common$selectors$goButton))
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
