context("Test Suite 2 (States) --> State 1.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

rD <- getRsDriver()
remDr <-rD$client

remDr$open(silent=TRUE)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr)
  })

  test_that("only state 1.1 is displayed", {
    checkDisplayedState(remDr, "1.1")
  })

  test_that("incidence data type buttons are displaying correctly", {
    # Check the div is displaying
    expect_true(isDisplayed(remDr, pages$state1.1$selectors$incidenceDataType))
    # Check the label is correct
    expect_true(isDisplayed(remDr, pages$state1.1$selectors$incidenceDataTypeLabel))
    expect_equal(getText(remDr, pages$state1.1$selectors$incidenceDataTypeLabel),
                 "Do you want to use pre-loaded incidence time series data or upload your own?")
    # Check the first radio input button (pre-loaded option)
    selector <- "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][1]"
    expect_equal(getAttribute(remDr, paste(selector, "//input", sep=""), "value"), "preloaded")
    expect_equal(getText(remDr, paste(selector, "//span", sep="")), "Pre-loaded")
    # Check the second radio input button (own data option)
    selector <- "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][2]"
    expect_equal(getAttribute(remDr, paste(selector, "//input", sep=""), "value"), "own")
    expect_equal(getText(remDr, paste(selector, "//span", sep="")), "Own data")
  })

  test_that("relevant control buttons are displayed", {
    expect_false(isDisplayed(remDr, pages$common$selectors$stopButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$prevButton))
    expect_false(isEnabled(remDr, pages$common$selectors$prevButton))
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
  remDr$close()
  rD$server$stop()
  stop(e)
})
remDr$close()
rD$server$stop()

