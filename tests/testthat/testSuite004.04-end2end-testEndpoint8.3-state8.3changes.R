context("Test Suite 4 (E2E) --> Endpoint 8.3")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 1 - Defaults                                                          #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.3 (Test 1)")
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
    click(remDr, pages$state6.1$selectors$SIDataTypeOwnButton)
    clickNext(remDr) # Move to state 7.2
    waitForStateDisplayed(remDr, "7.2")
    click(remDr, pages$state7.2$selectors$SIFromSampleButton)
    clickNext(remDr) # Move to state 8.3
    waitForStateDisplayed(remDr, "8.3")
    if (getAttribute(remDr, pages$state8.3$selectors$SISampleDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.3$selectors$SISampleDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SIPosteriorSamples/EcuadorRotavirus_SISamples_G.csv")
      sendKeys(remDr, pages$state8.3$selectors$SISampleDataUploadInput,
               path)
    }
    sendKeys(remDr, pages$state8.3$selectors$seedInput, "1")
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
I <- read.csv(paste(appDir, "datasets/IncidenceData/PennsylvaniaH1N1.csv", sep="/"), header=FALSE)
I <- EpiEstim:::process_I(I)
SI.Sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/EcuadorRotavirus_SISamples_G.csv", sep="/"), header=FALSE)
SI.Sample <- EpiEstim:::process_SI.Sample(SI.Sample)

epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, SI.Sample=SI.Sample, method="SIFromSample",
                         n2=100, seed=1)

test_that("Test 1 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})

# ---------------------------------------------------------------------------#
# Test 2 - Different Distribution                                            #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.3 (Test 2)")
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
    click(remDr, pages$state6.1$selectors$SIDataTypeOwnButton)
    clickNext(remDr) # Move to state 7.2
    waitForStateDisplayed(remDr, "7.2")
    click(remDr, pages$state7.2$selectors$SIFromSampleButton)
    clickNext(remDr) # Move to state 8.3
    waitForStateDisplayed(remDr, "8.3")
    if (getAttribute(remDr, pages$state8.3$selectors$SISampleDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.3$selectors$SISampleDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SIPosteriorSamples/EcuadorRotavirus_SISamples_W.csv")
      sendKeys(remDr, pages$state8.3$selectors$SISampleDataUploadInput,
               path)
    }
    sendKeys(remDr, pages$state8.3$selectors$seedInput, "1")
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
I <- read.csv(paste(appDir, "datasets/IncidenceData/PennsylvaniaH1N1.csv", sep="/"), header=FALSE)
I <- EpiEstim:::process_I(I)
SI.Sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/EcuadorRotavirus_SISamples_W.csv", sep="/"), header=FALSE)
SI.Sample <- EpiEstim:::process_SI.Sample(SI.Sample)

epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, SI.Sample=SI.Sample, method="SIFromSample",
                         n2=100, seed=1)

test_that("Test 2 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 3 - Different Dataset                                                 #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 8.3 (Test 3)")
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

  test_that("can walk through the app to endpoint state (Test 3)", {
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
    click(remDr, pages$state6.1$selectors$SIDataTypeOwnButton)
    clickNext(remDr) # Move to state 7.2
    waitForStateDisplayed(remDr, "7.2")
    click(remDr, pages$state7.2$selectors$SIFromSampleButton)
    clickNext(remDr) # Move to state 8.3
    waitForStateDisplayed(remDr, "8.3")
    if (getAttribute(remDr, pages$state8.3$selectors$SISampleDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.3$selectors$SISampleDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SIPosteriorSamples/NewYorkH1N1_SISamples_G.csv")
      sendKeys(remDr, pages$state8.3$selectors$SISampleDataUploadInput,
               path)
    }
    sendKeys(remDr, pages$state8.3$selectors$seedInput, "1")
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
I <- read.csv(paste(appDir, "datasets/IncidenceData/PennsylvaniaH1N1.csv", sep="/"), header=FALSE)
I <- EpiEstim:::process_I(I)
SI.Sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/NewYorkH1N1_SISamples_G.csv", sep="/"), header=FALSE)
SI.Sample <- EpiEstim:::process_SI.Sample(SI.Sample)

epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, SI.Sample=SI.Sample, method="SIFromSample",
                         n2=100, seed=1)

test_that("Test 3 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})
