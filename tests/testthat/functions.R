# This file contains lots of functions to be used in testing. 
# Most of them are wrappers to make the testthat syntax nicer, for example by removing all the [[1]]s.


library(RSelenium)

allStates = c("1.1", "2.1", "2.2", "3.1", "4.1", "5.1", "6.1", "6.2", "7.1", "7.2", "7.3", "7.4",
              "8.1", "8.2", "8.3", "8.4", "8.5", "9.1", "9.2", "9.3")
appUrl="http://localhost:3000"

xpaths <- list(
               nextButton="//div[@id='control']/button[@id='nxt']",
               prevButton="//div[@id='control']/button[@id='prev']",
               stopButton="//div[@id='control']/button[@id='stop']",
               goButton="//div[@id='control']/button[@id='go']",
               errorMessage="//div[@id='control']/div[@id='error']"
               )

findElem <- function(remDr, selector, using="xpath") {
  # Used instead of remDr$findElement(using="xpath", selector)
  return(remDr$findElement(using=using, selector))
}

findElems <- function(remDr, selector, using="xpath") {
  # Used instead of remDr$findElements(using="xpath", selector)
  return(remDr$findElements(using=using, selector))
}

getText <- function(remDr, selector) {
  webElem <- findElem(remDr, selector)
  # Used instead of webElem$getElementText()[[1]]
  return(webElem$getElementText()[[1]])
}

getAttribute <- function(remDr, selector, attr) {
  webElem <- findElem(remDr, selector)
  return(webElem$getElementAttribute(attr)[[1]])
}

isDisplayed <- function(remDr, selector) {
  webElem <- findElem(remDr, selector)
  return(webElem$isElementDisplayed()[[1]])
}

isEnabled <- function(remDr, selector) {
  webElem <- findElem(remDr, selector)
  return(webElem$isElementEnabled()[[1]])
}

isSelected <- function(selector) {
  webElem <- findElem(remDr, selector)
  return(webElem$isElementSelected()[[1]])
}

waitForElemDisplayed <- function(remDr, selector, timeout=10) {
  # Waits for webElem to be displayed for timeout seconds.
  # If passes if element is displayed within timeout, fails otherwise
  displayed <- FALSE
  tries <- 0
  while (!displayed & tries < timeout) {
    displayed <- isDisplayed(remDr, selector)
    tries <- tries + 1
  }
  expect_true(displayed)
}

connectToApp <- function(remDr) {
  # Navigates to the app, checks the title appears.
  remDr$navigate(appUrl)
  titleElem <- findElem(remDr, "//div[@id='incidenceTitle']")
  title <- titleElem$getElementText()[[1]]
  expect_equal(title, "Incidence Data")
}

waitForAppReady <- function(remDr, timeout=30) {
  # Waits for "Initialising..." to change to "Ready" for timeout seconds.
  initialising = TRUE
  tries=0
  while (initialising & tries < timeout) {
    statusSelector <- "//div[@id='status']"
    status <- getText(remDr, statusSelector)
    if (status == "Initialising...") {
      tries = tries + 1
      Sys.sleep(1)
    } else {
      initialising = FALSE
    }
  }
  expect_equal(status, "Ready")
}

checkDisplayedState <- function(remDr, expectedState) {
  # Checks that only expectedState is displayed.
  
  # Expected state should be displayed
  selector <- paste("//div[@id='", expectedState, "']", sep="")
  expect_true(isDisplayed(remDr, selector))
  # No other states should be displayed
  for (state in setdiff(allStates, expectedState)) {
    selector <- paste("//div[@id='", state, "']", sep="")
    expect_false(isDisplayed(remDr, selector))
  }
}

buildMatrix <- list(
  platforms=c("linux", "linux", "Windows 10", "Windows 10"),
  browserNames=c("firefox", "chrome", "firefox", "chrome")
)

getRsDriver <- function() {
  # Sets up a phantomjs remote driver using rsDriver.
  rDr <- rsDriver(remoteServerAddr = "localhost", port = 4444L, browser = "phantomjs"
                 , chromever = NULL, geckover = NULL, verbose=FALSE)
  return(rDr)
}

