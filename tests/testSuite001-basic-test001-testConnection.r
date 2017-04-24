context("Test Suite 1 (Basic) -------------> Connection      ")

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
remDr <-getRemoteDriver("Test Suite 1 (Basic) -> Connection", browser, platform)

remDr$open(silent=TRUE)
remDr$setWindowSize(1000,700)
appUrl="http://localhost:3000"
tryCatch({
	test_that("can connect to app", {
		connectToApp(remDr)
	})
  
  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr)
  })
  
  test_that("screenshot matches", {
    screenshotCompare(remDr, "1-basic001-initialScreenshot.png", update, browser, platform)
  })
},
error = function(e) {
	remDr$close()
	stop(e)
})
remDr$close()
