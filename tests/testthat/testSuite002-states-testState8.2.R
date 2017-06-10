context("Test Suite 2 (States) --> State 8.2")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 8.2")
rD <- drivers$rDr
remDr <- drivers$remDr

remDr$open(silent=TRUE)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr)
  })

  test_that("can navigate to state 8.2", {
   navigateToState(remDr, "8.2")
  })

  test_that("SI file upload buttons are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SIDataUploadLabel))
    expect_equal(getText(remDr, pages$state8.2$selectors$SIDataUploadLabel),
                 "Choose serialIntervalData file to upload")
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SIDataUploadBrowse))

    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SIHeaderButton))

    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SISepLabel))
    expect_equal(getText(remDr, pages$state8.2$selectors$SISepLabel), "Separator")
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SISepCommaButton))
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SISepCommaLabel))
    expect_equal(getText(remDr, pages$state8.2$selectors$SISepCommaLabel), "Comma")
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SISepSemiButton))
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SISepSemiLabel))
    expect_equal(getText(remDr, pages$state8.2$selectors$SISepSemiLabel), "Semicolon")
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SISepTabButton))
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SISepTabLabel))
    expect_equal(getText(remDr, pages$state8.2$selectors$SISepTabLabel), "Tab")

    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SIQuoteLabel))
    expect_equal(getText(remDr, pages$state8.2$selectors$SIQuoteLabel), "Quote")
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SIQuoteNoneButton))
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SIQuoteNoneLabel))
    expect_equal(getText(remDr, pages$state8.2$selectors$SIQuoteNoneLabel), "None")
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SIQuoteDoubleButton))
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SIQuoteDoubleLabel))
    expect_equal(getText(remDr, pages$state8.2$selectors$SIQuoteDoubleLabel), "Double Quote")
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SIQuoteSingleButton))
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$SIQuoteSingleLabel))
    expect_equal(getText(remDr, pages$state8.2$selectors$SIQuoteSingleLabel), "Single Quote")
  })

  test_that("seed input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$seedLabel))
    expect_equal(getText(remDr, pages$state8.2$selectors$seedLabel),
                 paste("Set a seed to be used by EpiEstim. A random one",
                       "will be chosen if this is left blank"))
    expect_true(isDisplayed(remDr, pages$state8.2$selectors$seedInput))
  })

  test_that("relevant control buttons are displayed", {
    expect_false(isDisplayed(remDr, pages$common$selectors$stopButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$prevButton))
    expect_true(isEnabled(remDr, pages$common$selectors$prevButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$nextButton))
    expect_true(isEnabled(remDr, pages$common$selectors$nextButton))
    expect_false(isDisplayed(remDr, pages$common$selectors$goButton))
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
