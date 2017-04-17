context("basic")

library(RSelenium)
library(testthat)
library(methods)


user <- Sys.getenv("SAUCE_USERNAME") # Your Sauce Labs username
pass <- Sys.getenv("SAUCE_ACCESS_KEY") # Your Sauce Labs access key 
port <- 4445 
ip <- paste0(user, ':', pass, "@localhost")
extraCapabilities <- list(name = "RSelenium OS/Browsers vignette first example", username = user
                          , accessKey = pass, tags = list("RSelenium-vignette", "OS/Browsers-vignette")
			  , startConnect = FALSE, tunnelIdentifier = Sys.getenv("TRAVIS_JOB_NUMBER"))


remDr <- remoteDriver$new(remoteServerAddr = ip, port = port, extraCapabilities = extraCapabilities)
remDr$open()
appUrl="http://localhost:3000"
tryCatch({
	test_that("can connect to app", {
		remDr$navigate(appUrl)
		titleElem <- remDr$findElement(using="id", "incidenceTitle")
		title <- titleElem$getElementText()
		expect_equal(title, "Incidence Data")
	})
},
error = function(e) {
	remDr$close()
	stop(e)
})
remDr$close()
