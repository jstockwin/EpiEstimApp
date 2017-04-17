runTests <- function () {
  env <- testthat::test_env()
  with(env, update <- FALSE)
  out<-testthat::test_dir('tests', reporter=c('summary', 'list', 'fail'), env=env);
  out
}

updateTests <- function () {
  env <- testthat::test_env()
  with(env, update <- TRUE)
  out<-testthat::test_dir('tests', reporter=c('summary', 'list', 'fail'), env=env);
  out
}


screenshotCompare <- function(remDr, filename, update) {
  # Take new screenshot
  remDr$screenshot(file=paste("../tests/current", filename, sep="/"))
  identical <- filesIdentical(filename)
  if (identical) {
    # Files are identical
    return(TRUE)
  } else if (!identical & update) {
    # Files are different, update the file
    remDr$screenshot(file=paste("../tests/expected", filename, sep="/"))
    return(TRUE)
  } else {
    # Files are not the same, and we're not updating tests
    return(FALSE)
  }
}


filesIdentical <- function(filename) {
  # Checks if the files are identical or not.
  a <- file.path("../tests/expected/", filename)
  b <- file.path("../tests/current/", filename)
  
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
    remDr <- remoteDriver$new(remoteServerAddr = ip, port = port, extraCapabilities = extraCapabilities)
  } else {
    remDr <- remoteDriver$new(remoteServerAddr = "localhost", port = 4444)
  }
  return(remDr)
}