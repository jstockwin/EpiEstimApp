# This file contains lots of functions to be used in testing. 
# Most of them are wrappers to make the testthat syntax nicer, for example by removing all the [[1]]s.


library(RSelenium)

allStates = c("1.1", "2.1", "2.2", "3.1", "4.1", "5.1", "6.1", "6.2", "7.1", "7.2", "7.3", "7.4",
              "8.1", "8.2", "8.3", "8.4", "8.5", "9.1", "9.2", "9.3")
appUrl="http://localhost:3000"

findElem <- function(remDr, selector, using="xpath") {
  # Used instead of remDr$findElement(using="xpath", selector)
  return(remDr$findElement(using=using, selector))
}

findElems <- function(remDr, selector, using="xpath") {
  # Used instead of remDr$findElements(using="xpath", selector)
  return(remDr$findElements(using=using, selector))
}

getText <- function(webElem) {
  # Used instead of webElem$getElementText()[[1]]
  return(webElem$getElementText()[[1]])
}

getAttribute <- function(webElem, attr) {
  return(webElem$getElementAttribute(attr)[[1]])
}

isDisplayed <- function(webElem) {
  return(webElem$isElementDisplayed()[[1]])
}

isEnabled <- function(webElem) {
  return(webElem$isElementEnabled()[[1]])
}

isSelected <- function(webElem) {
  return(webElem$isElementSelected()[[1]])
}

waitForElemDisplayed <- function(webElem, timeout=10) {
  # Waits for webElem to be displayed for timeout seconds.
  # If passes if element is displayed within timeout, fails otherwise
  displayed <- FALSE
  tries <- 0
  while (!displayed & tries < timeout) {
    displayed <- isDisplayed(webElem)
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
    statusElem <- findElem(remDr, "//div[@id='status']")
    status <- getText(statusElem)
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
  state <- findElem(remDr, selector)
  expect_true(isDisplayed(state))
  # No other states should be displayed
  for (state in setdiff(allStates, expectedState)) {
    selector <- paste("//div[@id='", state, "']", sep="")
    state <- findElem(remDr, selector)
    expect_false(isDisplayed(state))
  }
}

screenshotCompare <- function(remDr, filename, update, browserName, platform) {
  # Take new screenshot
  filenameCurrent <- paste("current", browserName, platform, filename, sep="/")
  filenameExpected <- paste("expected", browserName, platform, filename, sep="/")
  remDr$screenshot(file=filenameCurrent)
  identical <- filesIdentical(filenameCurrent, filenameExpected)
  match <- FALSE
  if (identical) {
    # Files are identical
    match <- TRUE
  } else if (!identical & update) {
    # Files are different, update the file
    cat("\nUpdating screenshot: ", filenameExpected, "\n")
    remDr$screenshot(file=filenameExpected)
    match <- TRUE
  }
  expect_true(match)
}


filesIdentical <- function(filenameCurrent, filenameExpected) {
  # Checks if the files are identical or not.
  a <- file.path(filenameCurrent)
  b <- file.path(filenameExpected)
  
  if (!file.exists(a)) {
    message("File ", a, " not found.")
    return(FALSE)
  }
  if (!file.exists(b)) {
    message("File ", b, " not found.")
    return(FALSE)
  }
  
  # Fast path: if not the same size, return FALSE
  a_size <- file.info(a)$size
  b_size <- file.info(b)$size
  if (!identical(a_size, b_size)) {
    return(FALSE)
  }
  
  a_content <- readBin(a, "raw", n = a_size)
  b_content <- readBin(b, "raw", n = b_size)
  return (identical(a_content, b_content))
}