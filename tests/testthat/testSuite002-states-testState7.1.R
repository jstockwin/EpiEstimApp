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
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$dataset_label))
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$dataset_option_1_label))
    expect_equal(getText(remDr, pages$state7.1$selectors$dataset_option_1_label),
                 "RotavirusEcuador2011")
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$dataset_option_1_input))
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$dataset_option_2_label))
    expect_equal(getText(remDr, pages$state7.1$selectors$dataset_option_2_label),
                 "H1N1NewYork2009")
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$dataset_option_2_input))
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$dataset_option_3_label))
    expect_equal(getText(remDr, pages$state7.1$selectors$dataset_option_3_label),
                 "H1N1USA2009")
    expect_true(isDisplayed(remDr, pages$state7.1$selectors$dataset_option_3_input))
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
    expect_false(isDisplayed(remDr, pages$common$selectors$error_message))
    expect_equal(getText(remDr, pages$common$selectors$error_message), "")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
