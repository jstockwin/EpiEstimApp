buildMatrix <- list(
  platforms=c("linux", "linux", "Windows 10", "Windows 10"),
  browserNames=c("firefox", "chrome", "firefox", "chrome")
)

runTests <- function (browsers="all", platforms="all", update=FALSE) {
  source("tests/functions.R")
  if (any(browsers=="all")) {
    browsers <- buildMatrix$browserNames
  }
  if (any(browsers=="fromEnv")) {
    browsers[which(browsers=="fromEnv")] <- Sys.getenv("browser")
  }
  if (any(platforms=="all")) {
    platforms <- buildMatrix$platforms
  }
  if (any(platforms=="fromEnv")) {
    platforms[which(platforms=="fromEnv")] <- Sys.getenv("platform")
  }
  numBrowsers <- length(browsers)
  for (i in 1:numBrowsers) {
    browser <- browsers[i]
    platform <- platforms[i]
    if (numBrowsers > 1) {
      cat("Beggining tests (pass", i, " of ", numBrowsers, ")\n")
      cat("Browser: ", browser, "\n")
      cat("platform: ", platform, "\n")
    }
    options(browser = browser, platform = platform, update = update)
    out<-testthat::test_dir('tests', reporter=c('summary', 'list', 'fail'));
    out
  }
}

getRemoteDriver <- function(name, browserName, platform) {		
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
                              , browserName = browserName, platform = platform)		
  } else {		
    remDr <- remoteDriver$new(remoteServerAddr = "localhost", port = 4444, browserName = browserName)		
  }		
  return(remDr)		
} 