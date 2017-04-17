context("basic")

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
		title <- titleElem$getElementText()
		expect_equal(title[[1]], "Incidence Data")
	})
},
error = function(e) {
	remDr$close()
	stop(e)
})
remDr$close()
