context("Test Suite 3 (Errors) --> State 8.3")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 3 (Errors) --> State 8.3")
rD <- drivers$rDr
remDr <- drivers$remDr

openRemDriver(remDr)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr)
  })

  test_that("can navigate to state 8.3", {
   navigateToState(remDr, "8.3")
  })

  test_that("Giving an invalid n2 throws correct error", {
    if (getAttribute(remDr, pages$state8.3$selectors$si_sample_data_upload_input, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.3$selectors$si_sample_data_upload_input, "style", "display: block;")
    }
    path <- getFilePath(remDr, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv")
    sendKeys(remDr, pages$state8.3$selectors$si_sample_data_upload_input,
             path)

    clear(remDr, pages$state8.3$selectors$n2_input)
    sendKeys(remDr, pages$state8.3$selectors$n2_input, "-1")
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n23")
    clear(remDr, pages$state8.3$selectors$n2_input)
    sendKeys(remDr, pages$state8.3$selectors$n2_input, "1.5")
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n23")
    # Reset for upcoming tests
    clear(remDr, pages$state8.3$selectors$n2_input)
    sendKeys(remDr, pages$state8.3$selectors$n2_input, "100")
  })

},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
