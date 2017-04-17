context("Test Suite 1, Basic")

library(RSelenium)
library(testthat)
source("../testUtils.R", local=TRUE)

# See utils.R. Set's up sauce connect on travis, or if the
# sauceUsername and sauceAccessKey are set in R
# Otherwise attempts to connect to a local selenium server on
# localhost:4444. Make sure you're running the app on
# port 3000 in a different process: `R -e "shiny::runApp(port=3000)`.
update <- getOption("update")
browser <- getOption("browser")
platform <- getOption("platform")
remDr <-getRemoteDriver("Running Test Connection", browser, platform)

remDr$open(silent=TRUE)
appUrl="http://localhost:3000"
tryCatch({
	test_that("can connect to app", {
		remDr$navigate(appUrl)
		titleElem <- remDr$findElement(using="id", "incidenceTitle")
		title <- titleElem$getElementText()[[1]]
		expect_equal(title, "Incidence Data")
	})
  
  test_that("app is ready within 60 seconds", {
    initialising = TRUE
    tries=0
    while (initialising & tries < 60) {
      statusElem <- remDr$findElement(using="id", "output")
      status <- statusElem$getElementText()[[1]]
      if (status == "Initialising...") {
        tries = tries + 1
        Sys.sleep(1)
      } else {
        initialising = FALSE
      }
    }
    expect_equal(status, "Ready")
  })
  
  test_that("screenshot matches", {
    expect_true(screenshotCompare(remDr, "1-basic001-initialScreenshot.png", update, browser, platform))
  })
},
error = function(e) {
	remDr$close()
	stop(e)
})
remDr$close()
