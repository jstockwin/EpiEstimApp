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
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$dataset_label))
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$dataset_option_1_label))
    expect_equal(getText(remDr, pages$state7.6$selectors$dataset_option_1_label),
                 "H1N1Maryland1918")
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$dataset_option_1_input))
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$dataset_option_2_label))
    expect_equal(getText(remDr, pages$state7.6$selectors$dataset_option_2_label),
                 "H1N1Pennsylvania2009")
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$dataset_option_2_input))
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$dataset_option_3_label))
    expect_equal(getText(remDr, pages$state7.6$selectors$dataset_option_3_label),
                 "MeaslesGermany1861")
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$dataset_option_3_input))
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$dataset_option_4_label))
    expect_equal(getText(remDr, pages$state7.6$selectors$dataset_option_4_label),
                 "SARSHongKong2003")
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$dataset_option_4_input))
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$dataset_option_5_label))
    expect_equal(getText(remDr, pages$state7.6$selectors$dataset_option_5_label),
                 "SmallpoxKosovo1972")
    expect_true(isDisplayed(remDr, pages$state7.6$selectors$dataset_option_5_input))
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
