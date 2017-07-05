context("Test Suite 2 (States) --> State 7.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 7.1")
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

  test_that("can navigate to state 7.1", {
    navigateToState(remDr, "7.1")
  })

  test_that("only state 7.1 is displayed", {
    checkDisplayedState(remDr, "7.1")
  })

  test_that("Dataset input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$datasetLabel))
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$datasetOption1Label))
    expect_equal(getText(remDr, pages$state7.1$selectors$datasetOption1Label),
                 "RotavirusEcuador2011")
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$datasetOption1Input))
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$datasetOption2Label))
    expect_equal(getText(remDr, pages$state7.1$selectors$datasetOption2Label),
                 "FluNewYork2009")
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$datasetOption2Input))
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$datasetOption3Label))
    expect_equal(getText(remDr, pages$state7.1$selectors$datasetOption3Label),
                 "FluUSA2009")
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$datasetOption3Input))
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
