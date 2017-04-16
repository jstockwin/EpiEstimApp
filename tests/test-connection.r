context("basic")

library(RSelenium)
library(testthat)


user <- Sys.getenv("SAUCE_USERNAME") # Your Sauce Labs username
pass <- Sys.getenv("SAUCE_ACCESS_KEY") # Your Sauce Labs access key 
port <- 4445 
ip <- paste0(user, ':', pass, "@localhost")
rdBrowser <- "chrome"
version <- "33"
platform <- "OS X 10.9"
extraCapabilities <- list(name = "RSelenium OS/Browsers vignette first example", username = user
                          , accessKey = pass, tags = list("RSelenium-vignette", "OS/Browsers-vignette")
			  , tunnelIdentifier = Sys.getenv("TRAVIS_JOB_NUMBER"), startConnect = FALSE)


remDr <- remoteDriver$new(remoteServerAddr = ip, port = port, browserName = rdBrowser
                          , version = version, platform = platform
                          , extraCapabilities = extraCapabilities)
remDr$open()

appUrl="http://localhost:6000"
test_that("can connect to app", {
	remDr$navigate(appUrl)
	appTitle <- remDr$getTitle()[[1]]
	expect_equal(appTitle, "http://localhost:6000/")
})

remDr$close()
