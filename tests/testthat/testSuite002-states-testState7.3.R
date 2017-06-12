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

  test_that("Mean.SI input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Mean.SILabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$Mean.SILabel),
                 "Mean.SI")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Mean.SIInput))
  })

  test_that("Std.Mean.SI input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Std.Mean.SILabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$Std.Mean.SILabel),
                 "Std.Mean.SI")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Std.Mean.SIInput))
  })

  test_that("Min.Mean.SI input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Min.Mean.SILabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$Min.Mean.SILabel),
                 "Min.Mean.SI")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Min.Mean.SIInput))
  })

  test_that("Max.Mean.SI input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Max.Mean.SILabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$Max.Mean.SILabel),
                 "Max.Mean.SI")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Max.Mean.SIInput))
  })

  test_that("Std.SI input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Std.SILabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$Std.SILabel),
                 "Std.SI")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Std.SIInput))
  })

  test_that("Std.Std.SI input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Std.Std.SILabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$Std.Std.SILabel),
                 "Std.Std.SI")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Std.Std.SIInput))
  })

  test_that("Min.Std.SI input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Min.Std.SILabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$Min.Std.SILabel),
                 "Min.Std.SI")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Min.Std.SIInput))
  })

  test_that("Max.Std.SI input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Max.Std.SILabel))
    expect_equal(getText(remDr, pages$state7.3$selectors$Max.Std.SILabel),
                 "Max.Std.SI")
    expect_true(isDisplayed(remDr, pages$state7.3$selectors$Max.Std.SIInput))
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
