context("Test Suite 2 (States) --> State 8.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 8.1")
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

  test_that("can navigate to state 8.1", {
   navigateToState(remDr, "8.1")
  })

  test_that("SI distribution selection input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionLabel))
    expect_equal(getText(remDr, pages$state8.1$selectors$distributionLabel),
                 "Choose your serial interval distribution")
    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption1Label))
    expect_equal(getText(remDr, pages$state8.1$selectors$distributionOption1Label),
                 "Gamma")
    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption1Input))

    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption2Label))
    expect_equal(getText(remDr, pages$state8.1$selectors$distributionOption2Label),
                 "Offset Gamma")
    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption2Input))

    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption3Label))
    expect_equal(getText(remDr, pages$state8.1$selectors$distributionOption3Label),
                 "Weibull")
    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption3Input))

    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption4Label))
    expect_equal(getText(remDr, pages$state8.1$selectors$distributionOption4Label),
                 "Offset Weibull")
    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption4Input))

    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption5Label))
    expect_equal(getText(remDr, pages$state8.1$selectors$distributionOption5Label),
                 "Log-Normal")
    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption5Input))

    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption6Label))
    expect_equal(getText(remDr, pages$state8.1$selectors$distributionOption6Label),
                 "Offset Log-Normal")
    expect_true(isDisplayed(remDr, pages$state8.1$selectors$distributionOption6Input))
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
