context("Test Suite 2 (States) --> State 2.2")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 2.2")
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

  test_that("can navigate to state 2.2", {
   navigateToState(remDr, "2.2")
  })

  test_that("incidence dataset selection input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_label))
    expect_equal(getText(remDr, pages$state2.2$selectors$dataset_label),
                 "Choose your dataset")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_1_label))
    expect_equal(getText(remDr, pages$state2.2$selectors$dataset_option_1_label),
                 "H1N1Pennsylvania2009")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_1_input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_2_label))
    expect_equal(getText(remDr, pages$state2.2$selectors$dataset_option_2_label),
                 "H1N1NewYork2009")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_2_input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_3_label))
    expect_equal(getText(remDr, pages$state2.2$selectors$dataset_option_3_label),
                 "RotavirusKiribati2013")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_3_input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_4_label))
    expect_equal(getText(remDr, pages$state2.2$selectors$dataset_option_4_label),
                 "H1N1Maryland1918")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_4_input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_5_label))
    expect_equal(getText(remDr, pages$state2.2$selectors$dataset_option_5_label),
                 "MeaslesGermany1861")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_5_input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_6_label))
    expect_equal(getText(remDr, pages$state2.2$selectors$dataset_option_6_label),
                 "SARSHongKong2003")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_6_input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_7_label))
    expect_equal(getText(remDr, pages$state2.2$selectors$dataset_option_7_label),
                 "SmallpoxKosovo1972")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$dataset_option_7_input))
  })

  test_that("width inputs are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$incidence_width_label))
    expect_equal(getText(remDr, pages$state2.2$selectors$incidence_width_label),
                 "Choose the length of the sliding time window, τ, over which R is estimated")
  })

  test_that("mean prior input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$mean_prior_label))
    expect_equal(getText(remDr, pages$state2.2$selectors$mean_prior_label),
                 "Choose the prior mean value for R")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$mean_prior_input))
  })

  test_that("std prior input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$std_prior_label))
    expect_equal(getText(remDr, pages$state2.2$selectors$std_prior_label),
                 "Choose the prior standard deviation value for R")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$std_prior_input))
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
