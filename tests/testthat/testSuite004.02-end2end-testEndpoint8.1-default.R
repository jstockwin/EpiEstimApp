context("Test Suite 4 (E2E) --> Endpoint 8.1")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 1 - Defaults                                                          #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 1)")
rD <- drivers$rDr
remDr <- drivers$remDr

appOut <- NULL
openRemDriver(remDr)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr)
  })

  test_that("can walk through the app to endpoint state (Test 1)", {
      # Walk the app through to endpoint state with default inputs
    click(remDr, pages$state1.1$selectors$preloadedDataButton)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$datasetOption1Input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposureDataYesInput)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$SIDataTypePreloadedButton)
    clickNext(remDr) # Move to state 7.1
    waitForStateDisplayed(remDr, "7.1")
    click(remDr, pages$state7.1$selectors$datasetOption1Input)
    clickNext(remDr) # Move to state 8.1
    waitForStateDisplayed(remDr, "8.1")
    click(remDr, pages$state8.1$selectors$distributionOption1Input)
    sendKeys(remDr, pages$state8.1$selectors$seedInput, "1")
    clickGo(remDr)
    Sys.sleep(1)
    waitForAppReady(remDr)

    appOut <<- extractOutputFromApp(remDr)
    closeRemDrivers(remDr, rD)
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})


test_that("Test 1 output matches", {
  # Compare the output to EpiEstim's output
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Pennsylvania2009.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_SI.Sample(sample)

  epiEstimOut <- EstimateR(I, T.Start=2:26, T.End=8:32, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)

  compareOutputFromApp(appOut, epiEstimOut)
})

