context("Test Suite 2 (States) --> State 6.2")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 6.2")
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

  test_that("can navigate to state 6.2", {
    navigateToState(remDr, "6.2")
  })

  test_that("only state 6.2 is displayed", {
    checkDisplayedState(remDr, "6.2")
  })

  test_that("uncertainty input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state6.2$selectors$uncertainty))
    expect_true(isDisplayed(remDr, pages$state6.2$selectors$uncertaintyLabel))
    expect_equal(getText(remDr, pages$state6.2$selectors$uncertaintyLabel),
                 "Would you like to include SI uncertainty in your model?")
    expect_true(isDisplayed(remDr, pages$state6.2$selectors$uncertaintyNoLabel))
    expect_equal(getText(remDr, pages$state6.2$selectors$uncertaintyNoLabel), "No")
    expect_true(isDisplayed(remDr, pages$state6.2$selectors$uncertaintyNoButton))
    expect_true(isDisplayed(remDr, pages$state6.2$selectors$uncertaintyYesLabel))
    expect_equal(getText(remDr, pages$state6.2$selectors$uncertaintyYesLabel), "Yes")
    expect_true(isDisplayed(remDr, pages$state6.2$selectors$uncertaintyYesButton))
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
