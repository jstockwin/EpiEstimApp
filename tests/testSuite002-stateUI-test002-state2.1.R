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
remDr <-getRemoteDriver("Test Suite 2 (State UI) -> State 2.1", browser, platform)

remDr$open(silent=TRUE)
remDr$setWindowSize(1000,700)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })
  
  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr)
  })
  
  test_that("can naviagate to state 2.1", {
    ownElem <- findElem(remDr, "//div[@id='incidenceDataType']//input[@value='own']")
    ownElem$clickElement()
    expect_true(isSelected(ownElem))
    
    nxtElem <- findElem(remDr, "//div[@id='control']/button[@id='nxt']")
    nxtElem$clickElement()
    state2.1 <- findElem(remDr, "//div[@id='2.1']")
    waitForElemDisplayed(state2.1)
  })
  
  test_that("title should be 'Incidence Data'", {
    webElem <- findElem(remDr, "//div[@id='titles']//h1")
    expect_equal(getText(webElem), "Incidence Data")
  })
  
  test_that("only state 2.1 is visible", {
    checkDisplayedState(remDr, "2.1")
  })
  
  test_that("incidence data upload field displays properly", {
    uploadLabel <- findElem(remDr, "//div[@id='2.1']//div[@id='incidenceDataErrorBox']/div/label[1]")
    expect_equal(getText(uploadLabel), "Choose incidence data file to upload")
    
    uploadButton <- findElem(remDr, "//div[@id='2.1']//div[@id='incidenceDataErrorBox']/div/div[1]/label/span")
    expect_equal(getText(uploadButton), "Browse...")
    
    headerCheckbox <- findElem(remDr, "//div[@id='2.1']//div[@class='checkbox']/label/input")
    expect_equal(getAttribute(headerCheckbox, "type"), "checkbox")
    
    seperatorLabel <- findElem(remDr, "//div[@id='2.1']//div[@id='incidenceSep']/label")
    expect_equal(getText(seperatorLabel), "Separator")
    seperatorOptions <- findElems(remDr, "//div[@id='2.1']//div[@id='incidenceSep']/div/div")
    expect_equal(length(seperatorOptions), 3)
    expect_equal(getText(seperatorOptions[[1]]), "Comma")
    expect_equal(getText(seperatorOptions[[2]]), "Semicolon")
    expect_equal(getText(seperatorOptions[[3]]), "Tab")
    
    quoteLabel <- findElem(remDr, "//div[@id='2.1']//div[@id='incidenceQuote']/label")
    expect_equal(getText(quoteLabel), "Quote")
    quoteOptions <- findElems(remDr, "//div[@id='2.1']//div[@id='incidenceQuote']/div/div")
    expect_equal(length(quoteOptions), 3)
    expect_equal(getText(quoteOptions[[1]]), "None")
    expect_equal(getText(quoteOptions[[2]]), "Double Quote")
    expect_equal(getText(quoteOptions[[3]]), "Single Quote")
  })
  
  test_that("the width input displays correctly", {
    widthLabel <- findElem(remDr, "//div[@id='2.1']//label[@for='uploadedWidth']")
    expect_equal(widthLabel$getElementText()[[1]], "Choose the width of the sliding time window for R estimation")
    
    widthInput <- findElem(remDr, "//div[@id='2.1']//input[@id='uploadedWidth']")
    expect_equal(widthInput$getElementAttribute("data-min")[[1]], "1")
    expect_equal(widthInput$getElementAttribute("data-max")[[1]], "20")
    expect_equal(widthInput$getElementAttribute("data-from")[[1]], "7")
    expect_equal(widthInput$getElementAttribute("data-step")[[1]], "1")
  })
  
  test_that("relevant control buttons are displayed", {
    stopElem <- findElem(remDr, "//div[@id='control']/button[@id='stop']")
    prevElem <- findElem(remDr, "//div[@id='control']/button[@id='prev']")
    nxtElem <- findElem(remDr, "//div[@id='control']/button[@id='nxt']")
    goElem <- findElem(remDr, "//div[@id='control']/button[@id='go']")
    
    expect_false(stopElem$isElementDisplayed()[[1]])
    expect_true(prevElem$isElementDisplayed()[[1]])
    expect_true(prevElem$isElementEnabled()[[1]])
    expect_true(nxtElem$isElementDisplayed()[[1]])
    expect_true(nxtElem$isElementEnabled()[[1]])
    expect_false(goElem$isElementDisplayed()[[1]])
  })
  
  test_that("no errors are displaying", {
    errorElem <- findElem(remDr, "//div[@id='control']/div[@id='error']")
    expect_true(errorElem$isElementDisplayed()[[1]])
    expect_equal(errorElem$getElementText()[[1]], "")
  })
  
  test_that("screenshot matches", {
    screenshotCompare(remDr, "2-state002-state2.1screenshot.png", update, browser, platform)
  })
},
error = function(e) {
  remDr$close()
  stop(e)
})
remDr$close()
