context("Test Suite 2 (State UI) ----------> State 1.1       ")

library(RSelenium)
library(testthat)
source("../testUtils.R", local=TRUE)

allStates = c("1.1", "2.1", "2.2", "3.1", "4.1", "5.1", "6.1", "6.2", "7.1", "7.2", "7.3", "7.4",
              "8.1", "8.2", "8.3", "8.4", "8.5", "9.1", "9.2", "9.3")

# See utils.R. Set's up sauce connect on travis, or if the
# sauceUsername and sauceAccessKey are set in R
# Otherwise attempts to connect to a local selenium server on
# localhost:4444. Make sure you're running the app on
# port 3000 in a different process: `R -e "shiny::runApp(port=3000)`.
update <- getOption("update")
browser <- getOption("browser")
platform <- getOption("platform")
remDr <-getRemoteDriver("Test Suite 2 (State UI) -> State 1.1", browser, platform)

remDr$open(silent=TRUE)
remDr$setWindowSize(1000,700)

tryCatch({
  # Tests state 1.1. We begin by checking that all control elements are on the page as they should be.
  test_that("can connect to app", {
    connectToApp(remDr)
  })
  
  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr)
  })
  
  test_that("title should be 'Incidence Data'", {
    webElem <- findElem(remDr, "//div[@id='titles']//h1")
    expect_equal(getText(webElem), "Incidence Data")
  })
  
  test_that("only state 1.1 should be displayed", {
    checkDisplayedState(remDr, "1.1")
  })
  
  test_that("incidence data type buttons are displaying correctly", {
    webElem <- findElem(remDr, "//div[@id='incidenceDataType']")
    expect_true(isDisplayed(webElem))
    
    webElem <- findElem(remDr, "//div[@id='incidenceDataType']/label")
    expect_true(isDisplayed(webElem))
    expect_equal(getText(webElem), "Do you want to use pre-loaded incidence time series data or upload your own?")
    
    # Check the first radio button input is correct (should be the pre-loaded option)
    webElem <- findElem(remDr, "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][1]//input")
    expect_equal(getAttribute(webElem, "value"), "preloaded")
    # Check the first radio button label is correct (should be the pre-loaded option)
    webElem <- findElem(remDr, "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][1]//span")
    expect_equal(getText(webElem), "Pre-loaded")
    
    # Check the second radio button input is correct (should be the own data option)
    webElem <- findElem(remDr, "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][2]//input")
    expect_equal(getAttribute(webElem, "value"), "own")
    # Check the second radio button label is correct (should be the own data option)
    webElem <- findElem(remDr, "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][2]//span")
    expect_equal(getText(webElem), "Own data")
  })
  
  test_that("relevant control buttons are displayed", {
    stopElem <- findElem(remDr, "//div[@id='control']/button[@id='stop']")
    prevElem <- findElem(remDr, "//div[@id='control']/button[@id='prev']")
    nxtElem <- findElem(remDr, "//div[@id='control']/button[@id='nxt']")
    goElem <- findElem(remDr, "//div[@id='control']/button[@id='go']")
    
    expect_false(isDisplayed(stopElem))
    expect_true(isDisplayed(prevElem))
    expect_false(isEnabled(prevElem))
    expect_true(isDisplayed(nxtElem))
    expect_true(isEnabled(nxtElem))
    expect_false(isDisplayed(stopElem))
  })
  
  test_that("no errors are displaying", {
    errorElem <- findElem(remDr, "//div[@id='control']/div[@id='error']")
    expect_true(isDisplayed(errorElem))
    expect_equal(getText(errorElem), "")
  })
  
  test_that("screenshot matches", {
    screenshotCompare(remDr, "2-state001-state1.1screenshot.png", update, browser, platform)
  })
},
error = function(e) {
  remDr$close()
  stop(e)
})
remDr$close()
