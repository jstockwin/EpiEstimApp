context("Test Suite 2 (States) --> State 8.3")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 8.3")
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

  test_that("can navigate to state 8.3", {
   navigateToState(remDr, "8.3")
  })

  test_that("SISample file upload buttons are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleDataUploadLabel))
    expect_equal(getText(remDr, pages$state8.3$selectors$SISampleDataUploadLabel),
                 "Choose serialIntervalData file to upload")
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleDataUploadBrowse))

    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleHeaderButton))

    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleSepLabel))
    expect_equal(getText(remDr, pages$state8.3$selectors$SISampleSepLabel), "Separator")
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleSepCommaButton))
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleSepCommaLabel))
    expect_equal(getText(remDr, pages$state8.3$selectors$SISampleSepCommaLabel), "Comma")
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleSepSemiButton))
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleSepSemiLabel))
    expect_equal(getText(remDr, pages$state8.3$selectors$SISampleSepSemiLabel), "Semicolon")
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleSepTabButton))
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleSepTabLabel))
    expect_equal(getText(remDr, pages$state8.3$selectors$SISampleSepTabLabel), "Tab")

    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleQuoteLabel))
    expect_equal(getText(remDr, pages$state8.3$selectors$SISampleQuoteLabel), "Quote")
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleQuoteNoneButton))
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleQuoteNoneLabel))
    expect_equal(getText(remDr, pages$state8.3$selectors$SISampleQuoteNoneLabel), "None")
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleQuoteDoubleButton))
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleQuoteDoubleLabel))
    expect_equal(getText(remDr, pages$state8.3$selectors$SISampleQuoteDoubleLabel), "Double Quote")
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleQuoteSingleButton))
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$SISampleQuoteSingleLabel))
    expect_equal(getText(remDr, pages$state8.3$selectors$SISampleQuoteSingleLabel), "Single Quote")
  })

  test_that("n2 input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$n2Label))
    expect_equal(getText(remDr, pages$state8.3$selectors$n2Label),
                 paste("Choose n2, the posterior sample size to be drawn for",
                       "R for each SI distribution sampled"))
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$n2Input))
  })

  test_that("seed input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$seedLabel))
    expect_equal(getText(remDr, pages$state8.3$selectors$seedLabel),
                 paste("Set a seed to be used by EpiEstim. A random one will",
                       "be chosen if this is left blank"))
    expect_true(isDisplayed(remDr, pages$state8.3$selectors$seedInput))
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
