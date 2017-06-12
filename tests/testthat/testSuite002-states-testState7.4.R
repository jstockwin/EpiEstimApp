context("Test Suite 2 (States) --> State 7.4")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 7.4")
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

  test_that("can navigate to state 7.4", {
    navigateToState(remDr, "7.4")
  })

  test_that("only state 7.4 is displayed", {
    checkDisplayedState(remDr, "7.4")
  })

  test_that("parametric input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.4$selectors$parametric))
    expect_true(isDisplayed(remDr, pages$state7.4$selectors$parametricLabel))
    expect_equal(getText(remDr, pages$state7.4$selectors$parametricLabel),
                 "Parametric or Non-Parametric SI?")
    expect_true(isDisplayed(remDr, pages$state7.4$selectors$parametricNoLabel))
    expect_equal(getText(remDr, pages$state7.4$selectors$parametricNoLabel), "Non-Parametric")
    expect_true(isDisplayed(remDr, pages$state7.4$selectors$parametricNoButton))
    expect_true(isDisplayed(remDr, pages$state7.4$selectors$parametricYesLabel))
    expect_equal(getText(remDr, pages$state7.4$selectors$parametricYesLabel), "Parametric")
    expect_true(isDisplayed(remDr, pages$state7.4$selectors$parametricYesButton))
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
