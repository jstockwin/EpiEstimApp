context("Test Suite 2 (States) --> State 2.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 2.1")
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

  test_that("can navigate to state 2.1", {
   navigateToState(remDr, "2.1")
  })

  test_that("incidence file upload buttons are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidence_data_upload_label))
    expect_equal(getText(remDr, pages$state2.1$selectors$incidence_data_upload_label),
                 "Choose incidence data file to upload")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceDataUpload_browse))

    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidence_header_button))

  })

  test_that("width inputs are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$uploaded_width_label))
    expect_equal(getText(remDr, pages$state2.1$selectors$uploaded_width_label),
                 "Choose the width of the sliding time window for R estimation")
  })

  test_that("mean prior input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$mean_prior_label))
    expect_equal(getText(remDr, pages$state2.1$selectors$mean_prior_label),
                 "Choose the prior mean value for R")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$mean_prior_input))
  })

  test_that("std prior input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$std_prior_label))
    expect_equal(getText(remDr, pages$state2.1$selectors$std_prior_label),
                 "Choose the prior standard deviation value for R")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$std_prior_input))
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
