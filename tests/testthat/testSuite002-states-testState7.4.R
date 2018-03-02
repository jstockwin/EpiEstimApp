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

  test_that("mean_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.4$selectors$mean_si_input))
    expect_true(isDisplayed(remDr, pages$state7.4$selectors$mean_si_label))
    expect_equal(getText(remDr, pages$state7.4$selectors$mean_si_label), "mean_si")
  })

  test_that("std_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.4$selectors$std_si_input))
    expect_true(isDisplayed(remDr, pages$state7.4$selectors$std_si_label))
    expect_equal(getText(remDr, pages$state7.4$selectors$std_si_label), "std_si")
  })

  test_that("relevant control buttons are displayed", {
    expect_false(isDisplayed(remDr, pages$common$selectors$stop_button))
    expect_true(isDisplayed(remDr, pages$common$selectors$prev_button))
    expect_true(isEnabled(remDr, pages$common$selectors$prev_button))
    expect_false(isDisplayed(remDr, pages$common$selectors$next_button))
    expect_true(isDisplayed(remDr, pages$common$selectors$go_button))
    expect_true(isEnabled(remDr, pages$common$selectors$go_button))
  })

  test_that("no errors are displaying", {
    expect_true(isDisplayed(remDr, pages$common$selectors$error_message))
    expect_equal(getText(remDr, pages$common$selectors$error_message), "")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
