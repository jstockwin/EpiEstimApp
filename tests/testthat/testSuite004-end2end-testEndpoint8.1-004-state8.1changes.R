context("Test Suite 4 (E2E) --> Endpoint 8.1")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 13 - Different SI.Dist (1)                                            #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 13)")
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

  test_that("can walk through the app to endpoint state (Test 13)", {
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
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)

# Compare the output to EpiEstim's output
I <- read.csv(paste(appDir, 'datasets/IncidenceData/PennsylvaniaH1N1.csv', sep='/'), header=FALSE)
I <- EpiEstim:::process_I(I)
sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/EcuadorRotavirus_SISamples_off1G.csv", sep="/"), header=FALSE)
sample <- EpiEstim:::process_SI.Sample(sample)

epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)

test_that("Test 13 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 14 - Different SI.Dist (2)                                            #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 14)")
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

  test_that("can walk through the app to endpoint state (Test 14)", {
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
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)

# Compare the output to EpiEstim's output
I <- read.csv(paste(appDir, 'datasets/IncidenceData/PennsylvaniaH1N1.csv', sep='/'), header=FALSE)
I <- EpiEstim:::process_I(I)
sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/EcuadorRotavirus_SISamples_W.csv", sep="/"), header=FALSE)
sample <- EpiEstim:::process_SI.Sample(sample)

epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)

test_that("Test 14 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 15 - Different SI.Dist (3)                                            #
# ---------------------------------------------------------------------------#

# DISABLED because the SI file is too big to process.
# See https://github.com/jstockwin/EpiEstimApp/issues/114

#drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 15)")
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
#  test_that("can walk through the app to endpoint state (Test 15)", {
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
#  })
#},
#error = function(e) {
#  closeRemDrivers(remDr, rD)
#  stop(e)
#})
#
#closeRemDrivers(remDr, rD)
#
## Compare the output to EpiEstim's output
#I <- read.csv(paste(appDir, 'datasets/IncidenceData/PennsylvaniaH1N1.csv', sep='/'), header=FALSE)
#I <- EpiEstim:::process_I(I)
#sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/EcuadorRotavirus_SISamples_off1W.csv", sep="/"), header=FALSE)
#sample <- EpiEstim:::process_SI.Sample(sample)
#
#epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)
#
#test_that("Test 15 output matches", {
#  compareOutputFromApp(appOut, epiEstimOut)
#})


# ---------------------------------------------------------------------------#
# Test 16 - Different SI.Dist (4)                                            #
# ---------------------------------------------------------------------------#

# DISABLED because the SI file is too big to process.
# See https://github.com/jstockwin/EpiEstimApp/issues/114

#drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 16)")
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
#  test_that("can walk through the app to endpoint state (Test 16)", {
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
#  })
#},
#error = function(e) {
#  closeRemDrivers(remDr, rD)
#  stop(e)
#})
#
#closeRemDrivers(remDr, rD)
#
## Compare the output to EpiEstim's output
#I <- read.csv(paste(appDir, 'datasets/IncidenceData/PennsylvaniaH1N1.csv', sep='/'), header=FALSE)
#I <- EpiEstim:::process_I(I)
#sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/EcuadorRotavirus_SISamples_L.csv", sep="/"), header=FALSE)
#sample <- EpiEstim:::process_SI.Sample(sample)
#
#epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)
#
#test_that("Test 16 output matches", {
#  compareOutputFromApp(appOut, epiEstimOut)
#})


# ---------------------------------------------------------------------------#
# Test 17 - Different SI.Dist (5)                                            #
# ---------------------------------------------------------------------------#

# DISABLED because the SI file is too big to process.
# See https://github.com/jstockwin/EpiEstimApp/issues/114

#drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 17)")
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
#  test_that("can walk through the app to endpoint state (Test 17)", {
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
#  })
#},
#error = function(e) {
#  closeRemDrivers(remDr, rD)
#  stop(e)
#})
#
#closeRemDrivers(remDr, rD)
#
## Compare the output to EpiEstim's output
#I <- read.csv(paste(appDir, 'datasets/IncidenceData/PennsylvaniaH1N1.csv', sep='/'), header=FALSE)
#I <- EpiEstim:::process_I(I)
#sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/EcuadorRotavirus_SISamples_off1L.csv", sep="/"), header=FALSE)
#sample <- EpiEstim:::process_SI.Sample(sample)
#
#epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)
#
#test_that("Test 17 output matches", {
#  compareOutputFromApp(appOut, epiEstimOut)
#})


# ---------------------------------------------------------------------------#
# Test 18 - Different n2                                                     #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 18)")
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

  test_that("can walk through the app to endpoint state (Test 18)", {
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
    sendKeys(remDr, pages$state8.1$selectors$n2Input, selKeys$backspace) # <---
    sendKeys(remDr, pages$state8.1$selectors$n2Input, selKeys$backspace) # <---
    sendKeys(remDr, pages$state8.1$selectors$n2Input, selKeys$backspace) # <---
    sendKeys(remDr, pages$state8.1$selectors$n2Input, "50") # <---
    sendKeys(remDr, pages$state8.1$selectors$seedInput, "1")
    clickGo(remDr)
    Sys.sleep(1)
    waitForAppReady(remDr)

    appOut <<- extractOutputFromApp(remDr)
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)

# Compare the output to EpiEstim's output
I <- read.csv(paste(appDir, 'datasets/IncidenceData/PennsylvaniaH1N1.csv', sep='/'), header=FALSE)
I <- EpiEstim:::process_I(I)
sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/EcuadorRotavirus_SISamples_G.csv", sep="/"), header=FALSE)
sample <- EpiEstim:::process_SI.Sample(sample)

epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, SI.Sample=sample, method="SIFromSample", n2=50, seed=1)

test_that("Test 18 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 19 - Different seed                                                   #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.1 (Test 19)")
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

  test_that("can walk through the app to endpoint state (Test 19)", {
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
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)

# Compare the output to EpiEstim's output
I <- read.csv(paste(appDir, 'datasets/IncidenceData/PennsylvaniaH1N1.csv', sep='/'), header=FALSE)
I <- EpiEstim:::process_I(I)
sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/EcuadorRotavirus_SISamples_G.csv", sep="/"), header=FALSE)
sample <- EpiEstim:::process_SI.Sample(sample)

epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, SI.Sample=sample, method="SIFromSample", n2=100, seed=2)

test_that("Test 19 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})


