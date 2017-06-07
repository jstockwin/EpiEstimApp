# This file contains lots of functions to be used in testing. 
# Most of them are wrappers to make the testthat syntax nicer, for example by removing all the [[1]]s.


library(RSelenium)

allStates = c("1.1", "2.1", "2.2", "3.1", "4.1", "5.1", "6.1", "6.2", "7.1", "7.2", "7.3", "7.4",
              "8.1", "8.2", "8.3", "8.4", "8.5", "9.1", "9.2", "9.3")
appUrl="http://localhost:3000"
source("pageObjects.R", local=TRUE)
appDir <- system.file("app", package="EpiEstimApp")

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

isSelected <- function(remDr, selector) {
  webElem <- findElem(remDr, selector)
  return(webElem$isElementSelected()[[1]])
}

sendKeys <- function(remDr, selector, keys) {
  webElem <- findElem(remDr, selector)
  webElem$sendKeysToElement(list(keys))
}

click <- function(remDr, selector) {
  webElem <- findElem(remDr, selector)
  webElem$clickElement()
}

clickNext <- function(remDr) {
  click(remDr, pages$common$selectors$nextButton)
}

clickPrev <- function(remDr) {
  click(remDr, pages$common$selectors$prevButton)
}

clickGo <- function(remDr) {
  click(remDr, pages$common$selectors$prevButton)
}

clickStop <- function(remDr) {
  click(remDr, pages$common$selectors$stopButton)
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
  title <- getText(remDr, pages$common$selectors$incidenceTitle)
  expect_equal(title, "Incidence Data")
}

waitForAppReady <- function(remDr, timeout=30) {
  # Waits for "Initialising..." to change to "Ready" for timeout seconds.
  initialising = TRUE
  tries=0
  while (initialising & tries < timeout) {
    status <- getText(remDr, pages$common$selectors$statusBar)
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

getDisplayedState <- function(remDr) {
  # Returns the currently displayed state
  for (state in allStates) {
    selector <- paste("//div[@id='", state, "']", sep="")
    if (isDisplayed(remDr, selector)) {
        return(state)
    }
  }
}

waitForStateDisplayed <- function(remDr, state) {
    selector <- paste("//div[@id='", state, "']", sep="")
    waitForElemDisplayed(remDr, selector)
}

navigateToState <- function(remDr, state) {
  # Navigate to state
  currentState <- getDisplayedState(remDr)
  switch(state,
         "1.1" = {
           count <- 0
           while (getDisplayedState(remDr) != "1.1") {
             clickPrev(remDr)
             if (count > 20) break
           }
         },
         "2.1" = {
           if (currentState=="1.1") {
             click(remDr, pages$state1.1$selectors$ownDataButton)
             clickNext(remDr)
           } else {
             navigateToState(remDr, "1.1")
             navigateToState(remDr, "2.1")
           }
         },
         "3.1" = {
           if (currentState=="2.1") {
             # We won't be able to move on unless we upload a
             # file...
             if (getAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "value") == "") {
               path <- paste(appDir, "/datasets/IncidenceData/PennsylvaniaH1N12009FluData.csv", sep="")
               sendKeys(remDr, pages$state2.1$selectors$incidenceDataUploadInput, path)
             }
             clickNext(remDr)
           } else {
             navigateToState(remDr, "2.1")
             navigateToState(remDr, "3.1")
           }
         }
  )
  waitForStateDisplayed(remDr, state)
}

buildMatrix <- list(
  platforms=c("linux", "linux", "Windows 10", "Windows 10"),
  browserNames=c("firefox", "chrome", "firefox", "chrome")
)

getRemDrivers <- function(name) {
  sauceLabs <- TRUE
      browserName = "firefox"
      platform = "linux"
      user <- Sys.getenv("SAUCE_USERNAME") # Your Sauce Labs username
      pass <- Sys.getenv("SAUCE_ACCESS_KEY") # Your Sauce Labs access key

  if (sauceLabs) {
    port <- 4445
    ip <- paste0(user, ':', pass, "@localhost")
    extraCapabilities <- list(name = name, username = user, accessKey = pass
                              , startConnect = FALSE, tunnelIdentifier = Sys.getenv("TRAVIS_JOB_NUMBER"))
    remDr <- remoteDriver$new(remoteServerAddr = ip, port = port, extraCapabilities = extraCapabilities
                              , browserName = browserName, platform = platform)	
    rDr <- NULL
  } else {
    rDr <- rsDriver(remoteServerAddr = "localhost", port = 4444L, verbose=FALSE)
    remDr <- rDr$client
  }
  return(list(remDr = remDr, rDr = rDr))
}

closeRemDrivers <- function(remDr, rDr) {
      remDr$close()
      rDr$server$stop()
}
