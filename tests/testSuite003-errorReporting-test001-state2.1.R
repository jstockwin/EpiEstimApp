context("Test Suite 2 (Error Reporting) ---> State 2.1       ")

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
  
  test_that("can naviagate to state 2.1", {
    ownElem <- remDr$findElement(using="xpath", "//div[@id='incidenceDataType']//input[@value='own']")
    ownElem$clickElement()
    expect_true(ownElem$isElementSelected()[[1]])
    
    nxtElem <- remDr$findElement(using="xpath", "//div[@id='control']/button[@id='nxt']")
    nxtElem$clickElement()
    state2.1 <- remDr$findElement(using="xpath", "//div[@id='2.1']")
    Sys.sleep(0.5) # Clicking next goes through sever, so is not instantaneous. 
    # TODO: Maybe create a waitForElementDisplayed(timeout) function?
    expect_true(state2.1$isElementDisplayed()[[1]])
  })
  
  test_that("clicking next without uploading a file reports an error", {
    nxtElem <- remDr$findElement(using="xpath", "//div[@id='control']/button[@id='nxt']")
    nxtElem$clickElement()
    
    # Check the state hasn't changed:
    state2.1 <- remDr$findElement(using="xpath", "//div[@id='2.1']")
    Sys.sleep(0.5) # Clicking next goes through sever, so is not instantaneous. 
    # TODO: Maybe create a waitForElementDisplayed(timeout) function?
    expect_true(state2.1$isElementDisplayed()[[1]])
    
    errorBox <- remDr$findElement(using="xpath", "//div[@id='2.1']//div[@id='incidenceDataErrorBox']")
    expect_equal(errorBox$getElementAttribute("style")[[1]], "border-style: solid;")
    
    errorMsg <- remDr$findElement(using="xpath", "//div[@id='control']/div[@id='error']")
    expect_true(errorMsg$isElementDisplayed()[[1]])
    expect_equal(errorMsg$getElementText()[[1]], "Please upload a file!")
    
    test_that("error screenshot matches", {
      expect_true(screenshotCompare(remDr, "3-errorReporting001-state2.1errorScreenshot.png", update, browser, platform))
    })
  })
},
error = function(e) {
  remDr$close()
  stop(e)
})
remDr$close()
