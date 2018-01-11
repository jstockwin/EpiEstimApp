context("Test Suite 2 (States) --> State 6.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 6.1")
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

  test_that("can navigate to state 6.1", {
    navigateToState(remDr, "6.1")
  })

  test_that("only state 6.1 is displayed", {
    checkDisplayedState(remDr, "6.1")
  })

  test_that("si_data_type input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$si_data_type))
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$si_data_type_label))
    expect_equal(getText(remDr, pages$state6.1$selectors$si_data_type_label),
                 paste("Would you like to use an external file containing",
                       "the exposure data, or a pre-loaded dataset?"))
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$si_data_type_preloaded_label))
    expect_equal(getText(remDr, pages$state6.1$selectors$si_data_type_preloaded_label), "Pre-loaded")
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$si_data_type_preloaded_button))
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$si_data_type_own_label))
    expect_equal(getText(remDr, pages$state6.1$selectors$si_data_type_own_label), "Own data")
    expect_true(isDisplayed(remDr, pages$state6.1$selectors$si_data_type_own_button))
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
