context("Test Suite 2 (States) --> State 4.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 4.1")
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

  test_that("can navigate to state 4.1", {
    navigateToState(remDr, "4.1")
  })

  test_that("only state 4.1 is displayed", {
    checkDisplayedState(remDr, "4.1")
  })

  test_that("imported file upload buttons are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$imported_data_upload_label))
    expect_equal(getText(remDr, pages$state4.1$selectors$imported_data_upload_label),
                 "Choose a data file with numbers of imported cases to upload")
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$imported_data_upload_browse))

    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedHeader_button))

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
