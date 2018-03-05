context("Test Suite 2 (States) --> State 6.2")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 6.2")
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

  test_that("can navigate to state 6.2", {
    navigateToState(remDr, "6.2")
  })

  test_that("only state 6.2 is displayed", {
    checkDisplayedState(remDr, "6.2")
  })

  test_that("si_est_type input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state6.2$selectors$si_est_type))
    expect_true(isDisplayed(remDr, pages$state6.2$selectors$si_est_type_label))
    expect_equal(getText(remDr, pages$state6.2$selectors$si_est_type_label),
                 "Which of the following serial interval distribution estimates would you like to use?")

    expect_true(isDisplayed(remDr, pages$state6.2$selector$si_est_type_option_1_label))
    expect_equal(getText(remDr, pages$state6.2$selector$si_est_type_option_1_label),
                 "Parametric with uncertainty (offset gamma)")

    expect_true(isDisplayed(remDr, pages$state6.2$selector$si_est_type_option_2_label))
    expect_equal(getText(remDr, pages$state6.2$selector$si_est_type_option_2_label),
                 "Parametric without uncertainty (offset gamma)")

    expect_true(isDisplayed(remDr, pages$state6.2$selector$si_est_type_option_3_label))
    expect_equal(getText(remDr, pages$state6.2$selector$si_est_type_option_3_label),
                 "Upload a probability distribution")

    expect_true(isDisplayed(remDr, pages$state6.2$selector$si_est_type_option_4_label))
    expect_equal(getText(remDr, pages$state6.2$selector$si_est_type_option_4_label),
                 "Use a distribution estimated from a previous outbreak (data in-built in app)")
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
