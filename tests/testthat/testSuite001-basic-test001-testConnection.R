context("Test Suite 1 (Basic) -------------> Connection      ")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

remDr <-getRemoteDriver("Test Suite 1 (Basic) -> Connection")

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
},
error = function(e) {
  remDr$close()
  stop(e)
})
remDr$close()