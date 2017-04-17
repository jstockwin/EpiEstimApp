context("Test Suite 2 (Error Reporting) ---> State 2.1       ")

library(RSelenium)
library(testthat)
source("../testUtils.R", local=TRUE)
source("functions.R", local=TRUE)

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
remDr <-getRemoteDriver("Test Suite 3 (Error Reporting) -> State 1.1", browser, platform)

remDr$open(silent=TRUE)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })
  
  test_that("app is ready within 60 seconds", {
    waitForAppReady(remDr)
  })
  
  test_that("can naviagate to state 2.1", {
    ownElem <- findElem(remDr, "//div[@id='incidenceDataType']//input[@value='own']")
    ownElem$clickElement()
    expect_true(ownElem$isElementSelected()[[1]])
    
    nxtElem <- findElem(remDr, "//div[@id='control']/button[@id='nxt']")
    nxtElem$clickElement()
    state2.1 <- findElem(remDr, "//div[@id='2.1']")
    waitForElemDisplayed(state2.1)
  })
  
  test_that("clicking next without uploading a file reports an error", {
    nxtElem <- findElem(remDr, "//div[@id='control']/button[@id='nxt']")
    nxtElem$clickElement()
    
    # Check the state hasn't changed:
    state2.1 <- findElem(remDr, "//div[@id='2.1']")
    Sys.sleep(0.5) # Need to actually wait here, can't do waitfor state 2.1 as it's already here!
    expect_true(isDisplayed(state2.1))
    
    errorBox <- findElem(remDr, "//div[@id='2.1']//div[@id='incidenceDataErrorBox']")
    expect_equal(getAttribute(errorBox, "style"), "border-style: solid;")
    
    errorMsg <- findElem(remDr, "//div[@id='control']/div[@id='error']")
    expect_true(isDisplayed(errorMsg))
    expect_equal(getText(errorMsg), "Please upload a file!")
    
    test_that("error screenshot matches", {
      screenshotCompare(remDr, "3-errorReporting001-state2.1errorScreenshot.png", update, browser, platform)
    })
  })
},
error = function(e) {
  remDr$close()
  stop(e)
})
remDr$close()
