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
    expect_true(isDisplayed(remDr, pages$state5.1$selectors$exposure_data_label))
    expect_equal(getText(remDr, pages$state5.1$selectors$exposure_data_label),
        paste("Do you want to use a",
            "distributional estimate of the",
            "serial interval, or use",
            "data from patients",
            "in known transmission chains?"))
    expect_true(isDisplayed(remDr, pages$state5.1$selectors$exposure_data_no_label))
    expect_equal(getText(remDr, pages$state5.1$selectors$exposure_data_no_label), "Distributional Estimate")
    expect_true(isDisplayed(remDr, pages$state5.1$selectors$exposure_data_no_input))
    expect_true(isDisplayed(remDr, pages$state5.1$selectors$exposure_data_yes_label))
    expect_equal(getText(remDr, pages$state5.1$selectors$exposure_data_yes_label), "Patient Data")
    expect_true(isDisplayed(remDr, pages$state5.1$selectors$exposure_data_yes_input))
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
