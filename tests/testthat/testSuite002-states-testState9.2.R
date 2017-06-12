context("Test Suite 2 (States) --> State 9.2")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 9.2")
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

  test_that("can navigate to state 9.2", {
   navigateToState(remDr, "9.2")
  })

  test_that("SIDistr file upload buttons are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrDataUploadLabel))
    expect_equal(getText(remDr, pages$state9.2$selectors$SIDistrDataUploadLabel),
                 "Choose serialIntervalData file to upload")
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrDataUploadBrowse))

    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrHeaderButton))

    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrSepLabel))
    expect_equal(getText(remDr, pages$state9.2$selectors$SIDistrSepLabel), "Separator")
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrSepCommaButton))
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrSepCommaLabel))
    expect_equal(getText(remDr, pages$state9.2$selectors$SIDistrSepCommaLabel), "Comma")
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrSepSemiButton))
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrSepSemiLabel))
    expect_equal(getText(remDr, pages$state9.2$selectors$SIDistrSepSemiLabel), "Semicolon")
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrSepTabButton))
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrSepTabLabel))
    expect_equal(getText(remDr, pages$state9.2$selectors$SIDistrSepTabLabel), "Tab")

    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrQuoteLabel))
    expect_equal(getText(remDr, pages$state9.2$selectors$SIDistrQuoteLabel), "Quote")
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrQuoteNoneButton))
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrQuoteNoneLabel))
    expect_equal(getText(remDr, pages$state9.2$selectors$SIDistrQuoteNoneLabel), "None")
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrQuoteDoubleButton))
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrQuoteDoubleLabel))
    expect_equal(getText(remDr, pages$state9.2$selectors$SIDistrQuoteDoubleLabel), "Double Quote")
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrQuoteSingleButton))
    expect_true(isDisplayed(remDr, pages$state9.2$selectors$SIDistrQuoteSingleLabel))
    expect_equal(getText(remDr, pages$state9.2$selectors$SIDistrQuoteSingleLabel), "Single Quote")
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
