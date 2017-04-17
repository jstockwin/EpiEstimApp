context("connection")

library(RSelenium)
library(testthat)
source("../utils.R", local=TRUE)

remDr <-prepSauceConnect("Running Test Connection")

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
},
error = function(e) {
	remDr$close()
	stop(e)
})
remDr$close()
