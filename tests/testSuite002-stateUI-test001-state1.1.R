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
remDr <-getRemoteDriver("Running Test Connection", browser, platform)

remDr$open(silent=TRUE)
appUrl="http://localhost:3000"
remDr$navigate(appUrl)
tryCatch({
  # Tests state 1.1. We begin by checking that all control elements are on the page as they should be.
  test_that("can connect to app", {
    remDr$navigate(appUrl)
    titleElem <- remDr$findElement(using="id", "incidenceTitle")
    title <- titleElem$getElementText()[[1]]
    expect_equal(title, "Incidence Data")
  })
  
  test_that("app is ready within 60 seconds", {
    initialising = TRUE
    tries=0
    while (initialising & tries < 60) {
      statusElem <- remDr$findElement(using="id", "output")
      status <- statusElem$getElementText()[[1]]
      if (status == "Initialising...") {
        tries = tries + 1
        Sys.sleep(1)
      } else {
        initialising = FALSE
      }
    }
    expect_equal(status, "Ready")
  })
  
  test_that("title should be 'Incidence Data'", {
    webElem <- remDr$findElement(using="xpath", "//div[@id='titles']//h1")
    expect_equal(webElem$getElementText()[[1]], "Incidence Data")
  })
  
  test_that("only state 1.1 should be displayed", {
    # State 1.1 should be displayed
    state1.1 <- remDr$findElement(using="xpath", "//div[@id='1.1']")
    expect_true(state1.1$isElementDisplayed()[[1]])
    # No other states should be displayed
    for (state in setdiff(allStates, "1.1")) {
      selector <- paste("//div[@id='", state, "']", sep="")
      state <- remDr$findElement(using="xpath", selector)
      expect_false(state$isElementDisplayed()[[1]])
    }
  })
  
  test_that("incidence data type buttons are displaying correctly", {
    webElem <- remDr$findElement(using="id", "incidenceDataType")
    expect_true(webElem$isElementDisplayed()[[1]])
    
    webElem <- remDr$findElement(using="xpath", "//div[@id='incidenceDataType']/label")
    expect_true(webElem$isElementDisplayed()[[1]])
    expect_equal(webElem$getElementText()[[1]], "Do you want to use pre-loaded incidence time series data or upload your own?")
    
    # Check the first radio button input is correct (should be the pre-loaded option)
    webElem <- remDr$findElement(using="xpath", "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][1]//input")
    expect_equal(webElem$getElementAttribute("value")[[1]], "preloaded")
    # Check the first radio button label is correct (should be the pre-loaded option)
    webElem <- remDr$findElement(using="xpath", "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][1]//span")
    expect_equal(webElem$getElementText()[[1]], "Pre-loaded")
    
    # Check the second radio button input is correct (should be the own data option)
    webElem <- remDr$findElement(using="xpath", "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][2]//input")
    expect_equal(webElem$getElementAttribute("value")[[1]], "own")
    # Check the second radio button label is correct (should be the own data option)
    webElem <- remDr$findElement(using="xpath", "//div[@id='incidenceDataType']/div[@class='shiny-options-group']/div[@class='radio'][2]//span")
    expect_equal(webElem$getElementText()[[1]], "Own data")
  })
  
  test_that("relevant control buttons are displayed", {
    stopElem <- remDr$findElement(using="xpath", "//div[@id='control']/button[@id='stop']")
    prevElem <- remDr$findElement(using="xpath", "//div[@id='control']/button[@id='prev']")
    nxtElem <- remDr$findElement(using="xpath", "//div[@id='control']/button[@id='nxt']")
    goElem <- remDr$findElement(using="xpath", "//div[@id='control']/button[@id='go']")
    
    expect_false(stopElem$isElementDisplayed()[[1]])
    expect_true(prevElem$isElementDisplayed()[[1]])
    expect_false(prevElem$isElementEnabled()[[1]])
    expect_true(nxtElem$isElementDisplayed()[[1]])
    expect_true(nxtElem$isElementEnabled()[[1]])
    expect_false(goElem$isElementDisplayed()[[1]])
  })
  
  test_that("no errors are displaying", {
    errorElem <- remDr$findElement(using="xpath", "//div[@id='control']/div[@id='error']")
    expect_true(errorElem$isElementDisplayed()[[1]])
    expect_equal(errorElem$getElementText()[[1]], "")
  })
  
  test_that("screenshot matches", {
    expect_true(screenshotCompare(remDr, "2-state001-state1.1screenshot.png", update, browser, platform))
  })
},
error = function(e) {
  remDr$close()
  stop(e)
})
remDr$close()
