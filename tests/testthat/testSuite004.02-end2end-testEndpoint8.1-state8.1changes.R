context("Test Suite 4 (E2E) --> Endpoint 8.1")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 1 - Different SI.Dist (1)                                            #
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
    click(remDr, pages$state8.1$selectors$distributionOption2Input) # <---
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
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_off1G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_SI.Sample(sample)

  epiEstimOut <- EstimateR(I, T.Start=2:26, T.End=8:32, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 2 - Different SI.Dist (2)                                            #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 2)")
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

  test_that("can walk through the app to endpoint state (Test 2)", {
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
    click(remDr, pages$state8.1$selectors$distributionOption3Input) # <---
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


test_that("Test 2 output matches", {
  # Compare the output to EpiEstim's output
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Pennsylvania2009.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_W.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_SI.Sample(sample)

  epiEstimOut <- EstimateR(I, T.Start=2:26, T.End=8:32, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 3 - Different SI.Dist (3)                                            #
# ---------------------------------------------------------------------------#

# DISABLED because the SI file is too big to process.
# See https://github.com/jstockwin/EpiEstimApp/issues/114

#drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 3)")
#rD <- drivers$rDr
#remDr <- drivers$remDr
#
#appOut <- NULL
#openRemDriver(remDr)
#tryCatch({
#  test_that("can connect to app", {
#    connectToApp(remDr)
#  })
#
#  test_that("app is ready within 30 seconds", {
#    waitForAppReady(remDr)
#  })
#
#  test_that("can walk through the app to endpoint state (Test 3)", {
#      # Walk the app through to endpoint state with default inputs
#    click(remDr, pages$state1.1$selectors$preloadedDataButton)
#    clickNext(remDr) # Move to state 2.2
#    waitForStateDisplayed(remDr, "2.2")
#    click(remDr, pages$state2.2$selectors$datasetOption1Input)
#    clickNext(remDr) # Move to state 5.1
#    waitForStateDisplayed(remDr, "5.1")
#    click(remDr, pages$state5.1$selectors$exposureDataYesInput)
#    clickNext(remDr) # Move to state 6.1
#    waitForStateDisplayed(remDr, "6.1")
#    click(remDr, pages$state6.1$selectors$SIDataTypePreloadedButton)
#    clickNext(remDr) # Move to state 7.1
#    waitForStateDisplayed(remDr, "7.1")
#    click(remDr, pages$state7.1$selectors$datasetOption1Input)
#    clickNext(remDr) # Move to state 8.1
#    waitForStateDisplayed(remDr, "8.1")
#    click(remDr, pages$state8.1$selectors$distributionOption4Input) # <---
#    sendKeys(remDr, pages$state8.1$selectors$seedInput, "1")
#    clickGo(remDr)
#    Sys.sleep(1)
#    waitForAppReady(remDr)
#
#    appOut <<- extractOutputFromApp(remDr)
#    closeRemDrivers(remDr, rD)
#  })
#},
#error = function(e) {
#  closeRemDrivers(remDr, rD)
#  stop(e)
#})
#
#
#test_that("Test 3 output matches", {
## Compare the output to EpiEstim's output
#I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Pennsylvania2009.csv', sep='/'), header=FALSE)
#I <- EpiEstim:::process_I(I)
#sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_off1W.csv", sep="/"), header=FALSE)
#sample <- EpiEstim:::process_SI.Sample(sample)
#
#epiEstimOut <- EstimateR(I, T.Start=2:26, T.End=8:32, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)
#
#  compareOutputFromApp(appOut, epiEstimOut)
#})


# ---------------------------------------------------------------------------#
# Test 4 - Different SI.Dist (4)                                            #
# ---------------------------------------------------------------------------#

# DISABLED because the SI file is too big to process.
# See https://github.com/jstockwin/EpiEstimApp/issues/114

#drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 4)")
#rD <- drivers$rDr
#remDr <- drivers$remDr
#
#appOut <- NULL
#openRemDriver(remDr)
#tryCatch({
#  test_that("can connect to app", {
#    connectToApp(remDr)
#  })
#
#  test_that("app is ready within 30 seconds", {
#    waitForAppReady(remDr)
#  })
#
#  test_that("can walk through the app to endpoint state (Test 4)", {
#      # Walk the app through to endpoint state with default inputs
#    click(remDr, pages$state1.1$selectors$preloadedDataButton)
#    clickNext(remDr) # Move to state 2.2
#    waitForStateDisplayed(remDr, "2.2")
#    click(remDr, pages$state2.2$selectors$datasetOption1Input)
#    clickNext(remDr) # Move to state 5.1
#    waitForStateDisplayed(remDr, "5.1")
#    click(remDr, pages$state5.1$selectors$exposureDataYesInput)
#    clickNext(remDr) # Move to state 6.1
#    waitForStateDisplayed(remDr, "6.1")
#    click(remDr, pages$state6.1$selectors$SIDataTypePreloadedButton)
#    clickNext(remDr) # Move to state 7.1
#    waitForStateDisplayed(remDr, "7.1")
#    click(remDr, pages$state7.1$selectors$datasetOption1Input)
#    clickNext(remDr) # Move to state 8.1
#    waitForStateDisplayed(remDr, "8.1")
#    click(remDr, pages$state8.1$selectors$distributionOption5Input) # <---
#    sendKeys(remDr, pages$state8.1$selectors$seedInput, "1")
#    clickGo(remDr)
#    Sys.sleep(1)
#    waitForAppReady(remDr)
#
#    appOut <<- extractOutputFromApp(remDr)
#    closeRemDrivers(remDr, rD)
#  })
#},
#error = function(e) {
#  closeRemDrivers(remDr, rD)
#  stop(e)
#})
#
#
#test_that("Test 4 output matches", {
## Compare the output to EpiEstim's output
#I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Pennsylvania2009.csv', sep='/'), header=FALSE)
#I <- EpiEstim:::process_I(I)
#sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_L.csv", sep="/"), header=FALSE)
#sample <- EpiEstim:::process_SI.Sample(sample)
#
#epiEstimOut <- EstimateR(I, T.Start=2:26, T.End=8:32, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)
#
#  compareOutputFromApp(appOut, epiEstimOut)
#})


# ---------------------------------------------------------------------------#
# Test 5 - Different SI.Dist (5)                                            #
# ---------------------------------------------------------------------------#

# DISABLED because the SI file is too big to process.
# See https://github.com/jstockwin/EpiEstimApp/issues/114

#drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 5)")
#rD <- drivers$rDr
#remDr <- drivers$remDr
#
#appOut <- NULL
#openRemDriver(remDr)
#tryCatch({
#  test_that("can connect to app", {
#    connectToApp(remDr)
#  })
#
#  test_that("app is ready within 30 seconds", {
#    waitForAppReady(remDr)
#  })
#
#  test_that("can walk through the app to endpoint state (Test 5)", {
#      # Walk the app through to endpoint state with default inputs
#    click(remDr, pages$state1.1$selectors$preloadedDataButton)
#    clickNext(remDr) # Move to state 2.2
#    waitForStateDisplayed(remDr, "2.2")
#    click(remDr, pages$state2.2$selectors$datasetOption1Input)
#    clickNext(remDr) # Move to state 5.1
#    waitForStateDisplayed(remDr, "5.1")
#    click(remDr, pages$state5.1$selectors$exposureDataYesInput)
#    clickNext(remDr) # Move to state 6.1
#    waitForStateDisplayed(remDr, "6.1")
#    click(remDr, pages$state6.1$selectors$SIDataTypePreloadedButton)
#    clickNext(remDr) # Move to state 7.1
#    waitForStateDisplayed(remDr, "7.1")
#    click(remDr, pages$state7.1$selectors$datasetOption1Input)
#    clickNext(remDr) # Move to state 8.1
#    waitForStateDisplayed(remDr, "8.1")
#    click(remDr, pages$state8.1$selectors$distributionOption6Input) # <---
#    sendKeys(remDr, pages$state8.1$selectors$seedInput, "1")
#    clickGo(remDr)
#    Sys.sleep(1)
#    waitForAppReady(remDr)
#
#    appOut <<- extractOutputFromApp(remDr)
#    closeRemDrivers(remDr, rD)
#  })
#},
#error = function(e) {
#  closeRemDrivers(remDr, rD)
#  stop(e)
#})
#
#
#test_that("Test 5 output matches", {
## Compare the output to EpiEstim's output
#I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Pennsylvania2009.csv', sep='/'), header=FALSE)
#I <- EpiEstim:::process_I(I)
#sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_off1L.csv", sep="/"), header=FALSE)
#sample <- EpiEstim:::process_SI.Sample(sample)
#
#epiEstimOut <- EstimateR(I, T.Start=2:26, T.End=8:32, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)
#
#  compareOutputFromApp(appOut, epiEstimOut)
#})


# ---------------------------------------------------------------------------#
# Test 6 - Different n2                                                     #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 6)")
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

  test_that("can walk through the app to endpoint state (Test 6)", {
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
    clear(remDr, pages$state8.1$selectors$n2Input) # <---
    sendKeys(remDr, pages$state8.1$selectors$n2Input, "50") # <---
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


test_that("Test 6 output matches", {
  # Compare the output to EpiEstim's output
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Pennsylvania2009.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_SI.Sample(sample)

  epiEstimOut <- EstimateR(I, T.Start=2:26, T.End=8:32, SI.Sample=sample, method="SIFromSample", n2=50, seed=1)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 7 - Different seed                                                   #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 7)")
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

  test_that("can walk through the app to endpoint state (Test 7)", {
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
    sendKeys(remDr, pages$state8.1$selectors$seedInput, "2") # <----
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


test_that("Test 7 output matches", {
  # Compare the output to EpiEstim's output
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Pennsylvania2009.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_SI.Sample(sample)

  epiEstimOut <- EstimateR(I, T.Start=2:26, T.End=8:32, SI.Sample=sample, method="SIFromSample", n2=100, seed=2)

  compareOutputFromApp(appOut, epiEstimOut)
})


