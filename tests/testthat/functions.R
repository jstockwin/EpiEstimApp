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

buildMatrix <- list(
  platforms=c("linux", "linux", "Windows 10", "Windows 10"),
  browserNames=c("firefox", "chrome", "firefox", "chrome")
)

getRemoteDriver <- function(name) {		
  # Set's up sauce connect on travis, or if the		
  # sauceUsername and sauceAccessKey are set in R		
  # Otherwise attempts to connect to a local selenium server on		
  # localhost:4444. Make sure you're running the app on		
  # port 3000 in a different process: `R -e "shiny::runApp(port=3000)`.		
  sauceLabs <- TRUE		
  if (!exists("sauceUsername")) {		
    if (Sys.getenv("SAUCE_USERNAME") != "") {		
      user <- Sys.getenv("SAUCE_USERNAME") # Your Sauce Labs username		
    } else {		
      sauceLabs <- FALSE		
    }		
  } else {		
    user <- sauceUsername		
  }		
  
  if (!exists("sauceAccessKey")) {		
    if (Sys.getenv("SAUCE_ACCESS_KEY") != "") {		
      pass <- Sys.getenv("SAUCE_ACCESS_KEY") # Your Sauce Labs access key		
    } else {		
      sauceLabs <- FALSE		
    }		
  } else {		
    pass <- sauceAccessKey		
  }		
  
  if (sauceLabs) {		
    port <- 4445 		
    ip <- paste0(user, ':', pass, "@localhost")		
    extraCapabilities <- list(name = name, username = user, accessKey = pass		
                              , startConnect = FALSE, tunnelIdentifier = Sys.getenv("TRAVIS_JOB_NUMBER"))		
    remDr <- remoteDriver$new(remoteServerAddr = ip, port = port, extraCapabilities = extraCapabilities		
                              , browserName = "firefox", platform = "linux")		
  } else {
    rDr <- rsDriver(remoteServerAddr = "localhost", port = 4444L, browser = "firefox")
    remDr <- rDr$client
  }		
  return(remDr)		
} 