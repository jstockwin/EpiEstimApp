context("Test Suite 2 (States) --> State 7.5")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 7.5")
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

  test_that("can navigate to state 7.5", {
   navigateToState(remDr, "7.5")
  })

  test_that("SIDistr file upload buttons are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state7.5$selectors$si_distr_data_upload_label))
    expect_equal(getText(remDr, pages$state7.5$selectors$si_distr_data_upload_label),
                 "Choose serial interval data file to upload")
    expect_true(isDisplayed(remDr, pages$state7.5$selectors$si_distr_data_upload_browse))

    expect_true(isDisplayed(remDr, pages$state7.5$selectors$si_distr_header_button))

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
