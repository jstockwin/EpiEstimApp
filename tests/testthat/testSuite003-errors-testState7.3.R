context("Test Suite 3 (Errors) --> State 7.3")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 3 (Errors) --> State 7.3")
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

  test_that("can navigate to state 7.3", {
   navigateToState(remDr, "7.3")
  })

  test_that("Giving an invalid n1 throws correct error", {
    clear(remDr, pages$state7.3$selectors$n1Input)
    sendKeys(remDr, pages$state7.3$selectors$n1Input, "-1")
    clickGo(remDr)
    checkError(remDr, "n1 must be an integer greater than or equal to 1", "n1")
    clear(remDr, pages$state7.3$selectors$n1Input)
    sendKeys(remDr, pages$state7.3$selectors$n1Input, "1.5")
    clickGo(remDr)
    checkError(remDr, "n1 must be an integer greater than or equal to 1", "n1")
    clear(remDr, pages$state7.3$selectors$n1Input)
    clickGo(remDr)
    checkError(remDr, "n1 must be an integer greater than or equal to 1", "n1")
    # Reset for upcoming tests
    clear(remDr, pages$state7.3$selectors$n1Input)
    sendKeys(remDr, pages$state7.3$selectors$n1Input, "50")
  })

  test_that("Giving an invalid n2 throws correct error", {
    clear(remDr, pages$state7.3$selectors$n2Input)
    sendKeys(remDr, pages$state7.3$selectors$n2Input, "-1")
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n2")
    clear(remDr, pages$state7.3$selectors$n2Input)
    sendKeys(remDr, pages$state7.3$selectors$n2Input, "1.5")
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n2")
    clear(remDr, pages$state7.3$selectors$n2Input)
    clickGo(remDr)
    checkError(remDr, "n2 must be an integer greater than or equal to 1", "n2")
    # Reset for upcoming tests
    clear(remDr, pages$state7.3$selectors$n2Input)
    sendKeys(remDr, pages$state7.3$selectors$n2Input, "50")
  })

  test_that("Giving an invalid Mean.SI throws correct error", {
    clear(remDr, pages$state7.3$selectors$Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Mean.SIInput, "-1")
    clickGo(remDr)
    checkError(remDr, "Mean.SI must be greater than or equal to 1", "Mean.SI")
    clear(remDr, pages$state7.3$selectors$Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Mean.SIInput, "0.99")
    clickGo(remDr)
    checkError(remDr, "Mean.SI must be greater than or equal to 1", "Mean.SI")
    clear(remDr, pages$state7.3$selectors$Mean.SIInput)
    clickGo(remDr)
    checkError(remDr, "Mean.SI must be greater than or equal to 1", "Mean.SI")
    # Reset
    clear(remDr, pages$state7.3$selectors$Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Mean.SIInput, "2")
  })

  test_that("Giving an invalid Min.Mean.SI throws correct error", {
    clear(remDr, pages$state7.3$selectors$Min.Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Min.Mean.SIInput, "-1")
    clickGo(remDr)
    checkError(remDr, "Min.Mean.SI must be greater than or equal to 1", "Min.Mean.SI")
    clear(remDr, pages$state7.3$selectors$Min.Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Min.Mean.SIInput, "0.99")
    clickGo(remDr)
    checkError(remDr, "Min.Mean.SI must be greater than or equal to 1", "Min.Mean.SI")
    clear(remDr, pages$state7.3$selectors$Min.Mean.SIInput)
    clickGo(remDr)
    checkError(remDr, "Min.Mean.SI must be greater than or equal to 1", "Min.Mean.SI")
    clear(remDr, pages$state7.3$selectors$Min.Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Min.Mean.SIInput, "2.5")
    clickGo(remDr)
    checkError(remDr, "Min.Mean.SI must be less than Mean.SI", "Mean.SI")
    checkError(remDr, "Min.Mean.SI must be less than Mean.SI", "Min.Mean.SI")
    # Reset
    clear(remDr, pages$state7.3$selectors$Min.Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Min.Mean.SIInput, "1")
  })

  test_that("Giving an invalid Max.Mean.SI throws correct error", {
    clear(remDr, pages$state7.3$selectors$Max.Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Max.Mean.SIInput, "-1")
    clickGo(remDr)
    checkError(remDr, "Max.Mean.SI must be greater than or equal to 1", "Max.Mean.SI")
    clear(remDr, pages$state7.3$selectors$Max.Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Max.Mean.SIInput, "0.99")
    clickGo(remDr)
    checkError(remDr, "Max.Mean.SI must be greater than or equal to 1", "Max.Mean.SI")
    clear(remDr, pages$state7.3$selectors$Max.Mean.SIInput)
    clickGo(remDr)
    checkError(remDr, "Max.Mean.SI must be greater than or equal to 1", "Max.Mean.SI")
    clear(remDr, pages$state7.3$selectors$Max.Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Max.Mean.SIInput, "1.5")
    clickGo(remDr)
    checkError(remDr, "Max.Mean.SI must be greater than Mean.SI", "Max.Mean.SI")
    checkError(remDr, "Max.Mean.SI must be greater than Mean.SI", "Mean.SI")
    # Reset
    clear(remDr, pages$state7.3$selectors$Max.Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Max.Mean.SIInput, "3")
  })

  test_that("Giving an invalid Std.Mean.SI throws correct error", {
    clear(remDr, pages$state7.3$selectors$Std.Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Std.Mean.SIInput, "-1")
    clickGo(remDr)
    checkError(remDr, "Std.Mean.SI must be greater than 0", "Std.Mean.SI")
    clear(remDr, pages$state7.3$selectors$Std.Mean.SIInput)
    clickGo(remDr)
    checkError(remDr, "Std.Mean.SI must be greater than 0", "Std.Mean.SI")
    # Reset
    clear(remDr, pages$state7.3$selectors$Std.Mean.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Std.Mean.SIInput, "1")
  })

  test_that("Giving an invalid Std.SI throws correct error", {
    clear(remDr, pages$state7.3$selectors$Std.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Std.SIInput, "-1")
    clickGo(remDr)
    checkError(remDr, "Std.SI must be greater than 0", "Std.SI")
    clear(remDr, pages$state7.3$selectors$Std.SIInput)
    clickGo(remDr)
    checkError(remDr, "Std.SI must be greater than 0", "Std.SI")
    # Reset
    clear(remDr, pages$state7.3$selectors$Std.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Std.SIInput, "2")
  })

  test_that("Giving an invalid Min.Std.SI throws correct error", {
    clear(remDr, pages$state7.3$selectors$Min.Std.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Min.Std.SIInput, "-1")
    clickGo(remDr)
    checkError(remDr, "Min.Std.SI must be greater than 0", "Min.Std.SI")
    clear(remDr, pages$state7.3$selectors$Min.Std.SIInput)
    clickGo(remDr)
    checkError(remDr, "Min.Std.SI must be greater than 0", "Min.Std.SI")
    clear(remDr, pages$state7.3$selectors$Min.Std.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Min.Std.SIInput, "2.5")
    clickGo(remDr)
    checkError(remDr, "Min.Std.SI must be less than Std.SI", "Std.SI")
    checkError(remDr, "Min.Std.SI must be less than Std.SI", "Min.Std.SI")
    # Reset
    clear(remDr, pages$state7.3$selectors$Min.Std.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Min.Std.SIInput, "1")
  })

  test_that("Giving an invalid Max.Std.SI throws correct error", {
    clear(remDr, pages$state7.3$selectors$Max.Std.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Max.Std.SIInput, "-1")
    clickGo(remDr)
    checkError(remDr, "Max.Std.SI must be greater than 0", "Max.Std.SI")
    clear(remDr, pages$state7.3$selectors$Max.Std.SIInput)
    clickGo(remDr)
    checkError(remDr, "Max.Std.SI must be greater than 0", "Max.Std.SI")
    clear(remDr, pages$state7.3$selectors$Max.Std.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Max.Std.SIInput, "1.5")
    clickGo(remDr)
    checkError(remDr, "Max.Std.SI must be greater than Std.SI", "Max.Std.SI")
    checkError(remDr, "Max.Std.SI must be greater than Std.SI", "Std.SI")
    # Reset
    clear(remDr, pages$state7.3$selectors$Max.Std.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Max.Std.SIInput, "3")
  })

  test_that("Giving an invalid Std.Std.SI throws correct error", {
    clear(remDr, pages$state7.3$selectors$Std.Std.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Std.Std.SIInput, "-1")
    clickGo(remDr)
    checkError(remDr, "Std.Std.SI must be greater than 0", "Std.Std.SI")
    clear(remDr, pages$state7.3$selectors$Std.Std.SIInput)
    clickGo(remDr)
    checkError(remDr, "Std.Std.SI must be greater than 0", "Std.Std.SI")
    # Reset
    clear(remDr, pages$state7.3$selectors$Std.Std.SIInput)
    sendKeys(remDr, pages$state7.3$selectors$Std.Std.SIInput, "1")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
