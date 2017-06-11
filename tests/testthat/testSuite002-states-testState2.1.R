context("Test Suite 2 (States) --> State 2.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 2.1")
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

  test_that("can navigate to state 2.1", {
   navigateToState(remDr, "2.1")
  })

  test_that("incidence file upload buttons are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceDataUploadLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$incidenceDataUploadLabel),
                 "Choose incidence data file to upload")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceDataUploadBrowse))

    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceHeaderButton))

    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceSepLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$incidenceSepLabel), "Separator")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceSepCommaButton))
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceSepCommaLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$incidenceSepCommaLabel), "Comma")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceSepSemiButton))
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceSepSemiLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$incidenceSepSemiLabel), "Semicolon")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceSepTabButton))
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceSepTabLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$incidenceSepTabLabel), "Tab")

    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceQuoteLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$incidenceQuoteLabel), "Quote")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceQuoteNoneButton))
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceQuoteNoneLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$incidenceQuoteNoneLabel), "None")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceQuoteDoubleButton))
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceQuoteDoubleLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$incidenceQuoteDoubleLabel), "Double Quote")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceQuoteSingleButton))
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$incidenceQuoteSingleLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$incidenceQuoteSingleLabel), "Single Quote")
  })

  test_that("width inputs are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$uploadedWidthLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$uploadedWidthLabel),
                 "Choose the width of the sliding time window for R estimation")
  })

  test_that("mean prior input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$meanPriorLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$meanPriorLabel),
                 "Choose the prior mean value for the reproduction number estimate")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$meanPriorInput))
  })

  test_that("std prior input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$stdPriorLabel))
    expect_equal(getText(remDr, pages$state2.1$selectors$stdPriorLabel),
                 "Choose the prior standard deviation value for the reproduction number estimate")
    expect_true(isDisplayed(remDr, pages$state2.1$selectors$stdPriorInput))
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
