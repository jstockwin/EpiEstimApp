context("Test Suite 2 (State UI) ----------> State 2.1       ")

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
  
  test_that("title should be 'Incidence Data'", {
    webElem <- remDr$findElement(using="xpath", "//div[@id='titles']//h1")
    expect_equal(webElem$getElementText()[[1]], "Incidence Data")
  })
  
  test_that("only state 2.1 is visible", {
    for (state in setdiff(allStates, "2.1")) {
      selector <- paste("//div[@id='", state, "']", sep="")
      state <- remDr$findElement(using="xpath", selector)
      expect_false(state$isElementDisplayed()[[1]])
    }
  })
  
  test_that("incidence data upload field displays properly", {
    uploadLabel <- remDr$findElement(using="xpath", "//div[@id='2.1']//div[@id='incidenceDataErrorBox']/div/label[1]")
    expect_equal(uploadLabel$getElementText()[[1]], "Choose incidence data file to upload")
    
    uploadButton <- remDr$findElement(using="xpath", "//div[@id='2.1']//div[@id='incidenceDataErrorBox']/div/div[1]/label/span")
    expect_equal(uploadButton$getElementText()[[1]], "Browse...")
    
    headerCheckbox <- remDr$findElement(using="xpath", "//div[@id='2.1']//div[@class='checkbox']/label/input")
    expect_equal(headerCheckbox$getElementAttribute("type")[[1]], "checkbox")
    
    seperatorLabel <- remDr$findElement(using="xpath", "//div[@id='2.1']//div[@id='incidenceSep']/label")
    expect_equal(seperatorLabel$getElementText()[[1]], "Separator")
    seperatorOptions <- remDr$findElements(using="xpath", "//div[@id='2.1']//div[@id='incidenceSep']/div/div")
    expect_equal(length(seperatorOptions), 3)
    expect_equal(seperatorOptions[[1]]$getElementText()[[1]], "Comma")
    expect_equal(seperatorOptions[[2]]$getElementText()[[1]], "Semicolon")
    expect_equal(seperatorOptions[[3]]$getElementText()[[1]], "Tab")
    
    quoteLabel <- remDr$findElement(using="xpath", "//div[@id='2.1']//div[@id='incidenceQuote']/label")
    expect_equal(quoteLabel$getElementText()[[1]], "Quote")
    quoteOptions <- remDr$findElements(using="xpath", "//div[@id='2.1']//div[@id='incidenceQuote']/div/div")
    expect_equal(length(quoteOptions), 3)
    expect_equal(quoteOptions[[1]]$getElementText()[[1]], "None")
    expect_equal(quoteOptions[[2]]$getElementText()[[1]], "Double Quote")
    expect_equal(quoteOptions[[3]]$getElementText()[[1]], "Single Quote")
  })
  
  test_that("the width input displays correctly", {
    widthLabel <- remDr$findElement(using="xpath", "//div[@id='2.1']//label[@for='uploadedWidth']")
    expect_equal(widthLabel$getElementText()[[1]], "Choose the width of the sliding time window for R estimation")
    
    widthInput <- remDr$findElement(using="xpath", "//div[@id='2.1']//input[@id='uploadedWidth']")
    expect_equal(widthInput$getElementAttribute("data-min")[[1]], "1")
    expect_equal(widthInput$getElementAttribute("data-max")[[1]], "20")
    expect_equal(widthInput$getElementAttribute("data-from")[[1]], "7")
    expect_equal(widthInput$getElementAttribute("data-step")[[1]], "1")
  })
  
  test_that("relevant control buttons are displayed", {
    stopElem <- remDr$findElement(using="xpath", "//div[@id='control']/button[@id='stop']")
    prevElem <- remDr$findElement(using="xpath", "//div[@id='control']/button[@id='prev']")
    nxtElem <- remDr$findElement(using="xpath", "//div[@id='control']/button[@id='nxt']")
    goElem <- remDr$findElement(using="xpath", "//div[@id='control']/button[@id='go']")
    
    expect_false(stopElem$isElementDisplayed()[[1]])
    expect_true(prevElem$isElementDisplayed()[[1]])
    expect_true(prevElem$isElementEnabled()[[1]])
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
    expect_true(screenshotCompare(remDr, "2-state002-state2.1screenshot.png", update, browser, platform))
  })
},
error = function(e) {
  remDr$close()
  stop(e)
})
remDr$close()
