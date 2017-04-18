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