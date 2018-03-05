context("Test Suite 2 (States) --> State 7.2")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 7.2")
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

  test_that("can navigate to state 7.2", {
    navigateToState(remDr, "7.2")
  })

  test_that("only state 7.2 is displayed", {
    checkDisplayedState(remDr, "7.2")
  })

  test_that("si_from input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$si_from))
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$si_from_label))
    expect_equal(getText(remDr, pages$state7.2$selectors$si_from_label),
    "Do you have data from patients in known transmission chains or a SI posterior sample to upload?")
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$si_from_raw_label))
    expect_equal(getText(remDr, pages$state7.2$selectors$si_from_raw_label), "Patient data")
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$si_from_raw_button))
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$si_from_sample_label))
    expect_equal(getText(remDr, pages$state7.2$selectors$si_from_sample_label), "SI posterior sample")
    expect_true(isDisplayed(remDr, pages$state7.2$selectors$si_from_sample_button))
  })

  test_that("relevant control buttons are displayed", {
    expect_false(isDisplayed(remDr, pages$common$selectors$stop_button))
    expect_true(isDisplayed(remDr, pages$common$selectors$prev_button))
    expect_true(isEnabled(remDr, pages$common$selectors$prev_button))
    expect_true(isDisplayed(remDr, pages$common$selectors$next_button))
    expect_true(isEnabled(remDr, pages$common$selectors$next_button))
    expect_false(isDisplayed(remDr, pages$common$selectors$go_button))
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
