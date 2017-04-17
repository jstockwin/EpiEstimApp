buildMatrix <- list(
  platforms=c("linux", "linux", "windows", "windows"),
  browserNames=c("firefox", "chrome", "firefox", "chrome")
)

runTests <- function (browsers="all", platforms="all", update=FALSE) {
  if (browsers=="all") {
    browsers <- buildMatrix$browserNames
  }
  if (any(browsers=="fromEnv")) {
    browsers[which(browsers=="fromEnv")] <- Sys.getenv("browser")
  }
  if (platforms=="all") {
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


screenshotCompare <- function(remDr, filename, update, browserName, platform) {
  # Take new screenshot
  filenameCurrent <- paste("../tests/current", browserName, platform, filename, sep="/")
  filenameExpected <- paste("../tests/expected", browserName, platform, filename, sep="/")
  remDr$screenshot(file=filenameCurrent)
  identical <- filesIdentical(filenameCurrent, filenameExpected)
  if (identical) {
    # Files are identical
    return(TRUE)
  } else if (!identical & update) {
    # Files are different, update the file
    cat("\nUpdating screenshot: ", filenameExpected, "\n")
    remDr$screenshot(file=filenameExpected)
    return(TRUE)
  } else {
    # Files are not the same, and we're not updating tests
    return(FALSE)
  }
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