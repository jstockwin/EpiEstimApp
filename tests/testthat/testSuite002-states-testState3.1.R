context("Test Suite 2 (States) --> State 3.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 3.1")
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

  test_that("can navigate to state 3.1", {
    navigateToState(remDr, "3.1")
  })

  test_that("only state 3.1 is displayed", {
    checkDisplayedState(remDr, "3.1")
  })

  test_that("imported input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state3.1$selectors$imported))
    expect_true(isDisplayed(remDr, pages$state3.1$selectors$importedLabel))
    expect_equal(getText(remDr, pages$state3.1$selectors$importedLabel),
                 "Do you have data about which cases are imported?")
    expect_true(isDisplayed(remDr, pages$state3.1$selectors$importedNoLabel))
    expect_equal(getText(remDr, pages$state3.1$selectors$importedNoLabel), "No")
    expect_true(isDisplayed(remDr, pages$state3.1$selectors$importedNoButton))
    expect_true(isDisplayed(remDr, pages$state3.1$selectors$importedYesLabel))
    expect_equal(getText(remDr, pages$state3.1$selectors$importedYesLabel), "Yes")
    expect_true(isDisplayed(remDr, pages$state3.1$selectors$importedYesButton))
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
