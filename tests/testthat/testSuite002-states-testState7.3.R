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
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$n1Label))
    expect_equal(getText(remDr, pages$state7.3$selectors$n1Label),
                 "n1")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$n1Input))
  })

  test_that("n2 input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$n2Label))
    expect_equal(getText(remDr, pages$state7.3$selectors$n2Label),
                 "n2")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$n2Input))
  })

  test_that("mean_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$mean_siLabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$mean_siLabel),
                 "mean_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$mean_siInput))
  })

  test_that("std_mean_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_mean_siLabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$std_mean_siLabel),
                 "std_mean_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_mean_siInput))
  })

  test_that("min_mean_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$min_mean_siLabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$min_mean_siLabel),
                 "min_mean_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$min_mean_siInput))
  })

  test_that("max_mean_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$max_mean_siLabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$max_mean_siLabel),
                 "max_mean_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$max_mean_siInput))
  })

  test_that("std_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_siLabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$std_siLabel),
                 "std_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_siInput))
  })

  test_that("std_std_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_std_siLabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$std_std_siLabel),
                 "std_std_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$std_std_siInput))
  })

  test_that("min_std_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$min_std_siLabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$min_std_siLabel),
                 "min_std_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$min_std_siInput))
  })

  test_that("max_std_si input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$max_std_siLabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$max_std_siLabel),
                 "max_std_si")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$max_std_siInput))
  })

  test_that("seed input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$seedLabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$seedLabel),
                 paste("Set a seed to be used by EpiEstim, so that the results",
                       "are reproducible. A random seed will be chosen if this",
                       "is left blank"))
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$seedInput))
  })

  test_that("relevant control buttons are displayed", {
    expect_false(isDisplayed(remDr, pages$common$selectors$stopButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$prevButton))
    expect_true(isEnabled(remDr, pages$common$selectors$prevButton))
    expect_false(isDisplayed(remDr, pages$common$selectors$nextButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$goButton))
    expect_true(isEnabled(remDr, pages$common$selectors$goButton))
  })

  test_that("no errors are displaying", {
    expect_true(isDisplayed(remDr, pages$common$selectors$errorMessage))
    expect_equal(getText(remDr, pages$common$selectors$errorMessage), "")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
