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
    expect_true(isDisplayed(remDr, pages$state3.1$selectors$imported_label))
    expect_equal(getText(remDr, pages$state3.1$selectors$imported_label),
                 "Do you have data about which cases are imported?")
    expect_true(isDisplayed(remDr, pages$state3.1$selectors$imported_no_label))
    expect_equal(getText(remDr, pages$state3.1$selectors$imported_no_label), "No")
    expect_true(isDisplayed(remDr, pages$state3.1$selectors$imported_no_button))
    expect_true(isDisplayed(remDr, pages$state3.1$selectors$imported_yes_label))
    expect_equal(getText(remDr, pages$state3.1$selectors$imported_yes_label), "Yes")
    expect_true(isDisplayed(remDr, pages$state3.1$selectors$imported_yes_button))
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
