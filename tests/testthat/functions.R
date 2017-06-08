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

setAttribute <- function(remDr, selector, attr, value) {
    webElem <- findElem(remDr, selector)
    webElem$setElementAttribute(attr, value)
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
  if (currentState == state) {
      return()
  }
  switch(state,
         "1.1" = {
           count <- 0
           while (getDisplayedState(remDr) != "1.1") {
             clickPrev(remDr)
             if (count > 20) break
           }
         },
         "2.1" = {
           navigateToState(remDr, "1.1")
           click(remDr, pages$state1.1$selectors$ownDataButton)
           clickNext(remDr)
         },
         "3.1" = {
           navigateToState(remDr, "2.1")
           # We won't be able to move on unless we upload a
           # file...
           if (getAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "value") == "") {
             # SAUCELABS gives an error about interacting with an element
             # which is not currently visible. Explicitly show the element
             # first to fix this?
             setAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "style", "display: block;")
             path <- getFilePath(remDr, "datasets/IncidenceData/PennsylvaniaH1N12009FluData.csv")
             sendKeys(remDr, pages$state2.1$selectors$incidenceDataUploadInput,
                      path)
           }
           clickNext(remDr)
         },
         "4.1" = {
           navigateToState(remDr, "3.1")
           click(remDr, pages$state3.1$selectors$importedYesButton)
           clickNext(remDr)
         }
  )
  waitForStateDisplayed(remDr, state)
}

buildMatrix <- list(
  platforms=c("linux", "linux", "Windows 10", "Windows 10"),
  browserNames=c("firefox", "chrome", "firefox", "chrome")
)

getRemDrivers <- function(name) {
  if (Sys.getenv("TRAVIS_JOB_NUMBER") == "") {
        rDr <- rsDriver(remoteServerAddr = "localhost", port = 4444L, verbose=FALSE,
                        browser="firefox")
        remDr <- rDr$client
  } else {
        browserName = Sys.getenv("browser")
        platform = Sys.getenv("platform")
        user <- Sys.getenv("SAUCE_USERNAME") # Your Sauce Labs username
        pass <- Sys.getenv("SAUCE_ACCESS_KEY") # Your Sauce Labs access key

        port <- 4445
        ip <- paste0(user, ':', pass, "@localhost")
        extraCapabilities <- list(name = name, username = user, accessKey = pass
                                  , startConnect = FALSE, tunnelIdentifier = Sys.getenv("TRAVIS_JOB_NUMBER"))
        remDr <- remoteDriver$new(remoteServerAddr = ip, port = port, extraCapabilities = extraCapabilities
                                  , browserName = browserName, platform = platform) 
        rDr <- NULL
  }
  return(list(remDr = remDr, rDr = rDr))
}

closeRemDrivers <- function(remDr, rDr) {
    tryCatch({
      remDr$close()
      rDr$server$stop()
    },
    error = function(e) {
      # Ignore errors when rDr is null
    })
}

getFilePath <- function(remDr, file) {
   # file should be a path relative to the apps root directory
   # Returns full file path on most systems, or uploads to saucelabs
   # and returns the path on saucelabs if needed.
   path <- paste(appDir, "/", file, sep="")
   if (Sys.getenv("TRAVIS_JOB_NUMBER")=="") {
       return(path)
   } else {
       # saucelabsPath will be the path to the home directory on saucelabs
       # so we take substring(path, 6) which removes the /home/ from path
       # to avoid having it twice.
       saucelabsPath <- myUpload(remDr, path)
       newPath <- paste0(saucelabsPath, substring(path, 6), sep="")
       return(newPath)
   }
}

myUpload <- function(remDr, myfile){
  # Uploads file to saucelabs and returns the path of the home folder
  tmpfile <- tempfile(fileext = ".zip")
  # zip file
  zip(tmpfile, myfile)
  # base64 encode
  zz <- file(tmpfile, "rb")
  ar <- readBin(tmpfile, "raw", file.info(tmpfile)$size)
  encFile <- caTools::base64encode(ar)
  close(zz)
  qpath <- sprintf("%s/session/%s/file", remDr$serverURL, 
                   remDr$sessionInfo[["id"]])
  res <- queryRD(qpath, "POST", qdata = list(file = encFile))
  # return result from server
  httr::content(res, simplifyVector = FALSE)$value
}

# The following functions is used in the above FileUpload function
# and are taken from:
# https://github.com/ropensci/RSelenium/blob/539660780454c321822b37e4c39a321a34528b0d/R/errorHandler.R
# See https://github.com/ropensci/RSelenium/issues/130

# TODO: Error handling/status code checking etc
queryRD = 
        function(ipAddr, method = "GET", qdata = NULL){
          "A method to communicate with the remote server implementing the 
          JSON wire protocol."
        getUC.params <- 
          list(url = ipAddr, verb = method, body = qdata, encode = "json")
        res <- tryCatch(
          {do.call(httr::VERB, getUC.params)}, 
          error = function(e){e}
        )
res}

