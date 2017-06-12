context("Test Suite 2 (States) --> State 4.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 4.1")
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

  test_that("can navigate to state 4.1", {
    navigateToState(remDr, "4.1")
  })

  test_that("only state 4.1 is displayed", {
    checkDisplayedState(remDr, "4.1")
  })

  test_that("imported file upload buttons are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedDataUploadLabel))
    expect_equal(getText(remDr, pages$state4.1$selectors$importedDataUploadLabel),
                 "Choose a data file with numbers of imported cases to upload")
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedDataUploadBrowse))

    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedHeaderButton))

    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedSepLabel))
    expect_equal(getText(remDr, pages$state4.1$selectors$importedSepLabel), "Separator")
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedSepCommaButton))
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedSepCommaLabel))
    expect_equal(getText(remDr, pages$state4.1$selectors$importedSepCommaLabel), "Comma")
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedSepSemiButton))
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedSepSemiLabel))
    expect_equal(getText(remDr, pages$state4.1$selectors$importedSepSemiLabel), "Semicolon")
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedSepTabButton))
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedSepTabLabel))
    expect_equal(getText(remDr, pages$state4.1$selectors$importedSepTabLabel), "Tab")

    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedQuoteLabel))
    expect_equal(getText(remDr, pages$state4.1$selectors$importedQuoteLabel), "Quote")
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedQuoteNoneButton))
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedQuoteNoneLabel))
    expect_equal(getText(remDr, pages$state4.1$selectors$importedQuoteNoneLabel), "None")
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedQuoteDoubleButton))
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedQuoteDoubleLabel))
    expect_equal(getText(remDr, pages$state4.1$selectors$importedQuoteDoubleLabel), "Double Quote")
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedQuoteSingleButton))
    expect_true(isDisplayed(remDr, pages$state4.1$selectors$importedQuoteSingleLabel))
    expect_equal(getText(remDr, pages$state4.1$selectors$importedQuoteSingleLabel), "Single Quote")
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
