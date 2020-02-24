context("Test Suite 2 (States) --> State 7.3")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 7.3")
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

  test_that("can navigate to state 7.3", {
   navigateToState(remDr, "7.3")
  })

  test_that("n1 input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$n1_label))
    expect_equal(getText(remDr, pages$state7.3$selectors$n1_label),
                 "n1")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$n1_input))
  })

  test_that("n2 input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$n2_label))
    expect_equal(getText(remDr, pages$state7.3$selectors$n2_label),
                 "n2")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$n2_input))
  })

  test_that("mean_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$mean_si_label))
    expect_equal(getText(remDr, pages$state7.3$selectors$mean_si_label),
                 "mean_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$mean_si_input))
  })

  test_that("std_mean_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_mean_si_label))
    expect_equal(getText(remDr, pages$state7.3$selectors$std_mean_si_label),
                 "std_mean_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_mean_si_input))
  })

  test_that("min_mean_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$min_mean_si_label))
    expect_equal(getText(remDr, pages$state7.3$selectors$min_mean_si_label),
                 "min_mean_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$min_mean_si_input))
  })

  test_that("max_mean_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$max_mean_si_label))
    expect_equal(getText(remDr, pages$state7.3$selectors$max_mean_si_label),
                 "max_mean_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$max_mean_si_input))
  })

  test_that("std_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_si_label))
    expect_equal(getText(remDr, pages$state7.3$selectors$std_si_label),
                 "std_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_si_input))
  })

  test_that("std_std_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_std_si_label))
    expect_equal(getText(remDr, pages$state7.3$selectors$std_std_si_label),
                 "std_std_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_std_si_input))
  })

  test_that("min_std_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$min_std_si_label))
    expect_equal(getText(remDr, pages$state7.3$selectors$min_std_si_label),
                 "min_std_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$min_std_si_input))
  })

  test_that("max_std_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$max_std_si_label))
    expect_equal(getText(remDr, pages$state7.3$selectors$max_std_si_label),
                 "max_std_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$max_std_si_input))
  })

  test_that("seed input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$seed_label))
    expect_equal(getText(remDr, pages$state7.3$selectors$seed_label),
                 paste("Set a seed to be used by EpiEstim, so that the results",
                       "are reproducible. A random seed will be chosen if this",
                       "is left blank"))
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$seed_input))
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
    expect_false(isDisplayed(remDr, pages$common$selectors$error_message))
    expect_equal(getText(remDr, pages$common$selectors$error_message), "")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
