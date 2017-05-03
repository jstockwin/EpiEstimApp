context("Test Suite 1 (Basic) --> Connection")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

rD <- getRsDriver()
remDr <- rD$client

remDr$open(silent=TRUE)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr)
  })
},
error = function(e) {
  remDr$close()
  rD$server$stop()
  stop(e)
})
remDr$close()
rD$server$stop()

