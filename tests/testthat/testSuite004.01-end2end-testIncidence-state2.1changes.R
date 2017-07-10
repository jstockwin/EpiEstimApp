context("Test Suite 4 (E2E) --> Incidence")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 1 - Default                                                           #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Incidence (Test 1)")
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
    click(remDr, pages$state1.1$selectors$ownDataButton)
    clickNext(remDr) # Move to state 2.1
    waitForStateDisplayed(remDr, "2.1")
    if (getAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/IncidenceData/H1N1Pennsylvania2009.csv")
      sendKeys(remDr, pages$state2.1$selectors$incidenceDataUploadInput,
               path)
    }
    clickNext(remDr) # Move to state 3.1
    waitForStateDisplayed(remDr, "3.1")
    click(remDr, pages$state3.1$selectors$importedNoButton)
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


# ---------------------------------------------------------------------------#
# Test 2 - Different Incidence Data (1)                                      #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Incidence (Test 2)")
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
    click(remDr, pages$state1.1$selectors$ownDataButton)
    clickNext(remDr) # Move to state 2.1
    waitForStateDisplayed(remDr, "2.1")
    if (getAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/IncidenceData/RotavirusKiribati2013.csv") # <---
      sendKeys(remDr, pages$state2.1$selectors$incidenceDataUploadInput,
               path)
    }
    clickNext(remDr) # Move to state 3.1
    waitForStateDisplayed(remDr, "3.1")
    click(remDr, pages$state3.1$selectors$importedNoButton)
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


test_that("Test 2 output matches", {
  # Compare the output to EpiEstim's output
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/RotavirusKiribati2013.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_SI.Sample(sample)

  epiEstimOut <- EstimateR(I, T.Start=2:17, T.End=8:23, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 3 - Different Incidence Data (2)                                      #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Incidence (Test 3)")
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
    click(remDr, pages$state1.1$selectors$ownDataButton)
    clickNext(remDr) # Move to state 2.1
    waitForStateDisplayed(remDr, "2.1")
    if (getAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/IncidenceData/H1N1NewYork2009.csv") # <---
      sendKeys(remDr, pages$state2.1$selectors$incidenceDataUploadInput,
               path)
    }
    clickNext(remDr) # Move to state 3.1
    waitForStateDisplayed(remDr, "3.1")
    click(remDr, pages$state3.1$selectors$importedNoButton)
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


test_that("Test 3 output matches", {
  # Compare the output to EpiEstim's output
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1NewYork2009.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_SI.Sample(sample)

  epiEstimOut <- EstimateR(I, T.Start=2:8, T.End=8:14, SI.Sample=sample, method="SIFromSample", n2=100, seed=1)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 4 - Different Mean.Prior                                              #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Incidence (Test 4)")
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

  test_that("can walk through the app to endpoint state (Test 4)", {
      # Walk the app through to endpoint state with default inputs
    click(remDr, pages$state1.1$selectors$ownDataButton)
    clickNext(remDr) # Move to state 2.1
    waitForStateDisplayed(remDr, "2.1")
    if (getAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/IncidenceData/H1N1Pennsylvania2009.csv")
      sendKeys(remDr, pages$state2.1$selectors$incidenceDataUploadInput,
               path)
    }
    clear(remDr, pages$state2.1$selectors$meanPriorInput) # <---
    sendKeys(remDr, pages$state2.1$selectors$meanPriorInput, "6") # <---
    clickNext(remDr) # Move to state 3.1
    waitForStateDisplayed(remDr, "3.1")
    click(remDr, pages$state3.1$selectors$importedNoButton)
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


test_that("Test 4 output matches", {
  # Compare the output to EpiEstim's output
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Pennsylvania2009.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_SI.Sample(sample)

  epiEstimOut <- EstimateR(I, T.Start=2:26, T.End=8:32, SI.Sample=sample, method="SIFromSample", n2=100, seed=1, Mean.Prior=6)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 5 - Different Std.Prior                                               #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Incidence (Test 5)")
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

  test_that("can walk through the app to endpoint state (Test 5)", {
      # Walk the app through to endpoint state with default inputs
    click(remDr, pages$state1.1$selectors$ownDataButton)
    clickNext(remDr) # Move to state 2.1
    waitForStateDisplayed(remDr, "2.1")
    if (getAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state2.1$selectors$incidenceDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/IncidenceData/H1N1Pennsylvania2009.csv")
      sendKeys(remDr, pages$state2.1$selectors$incidenceDataUploadInput,
               path)
    }
    clear(remDr, pages$state2.1$selectors$stdPriorInput) # <---
    sendKeys(remDr, pages$state2.1$selectors$stdPriorInput, "6") # <---
    clickNext(remDr) # Move to state 3.1
    waitForStateDisplayed(remDr, "3.1")
    click(remDr, pages$state3.1$selectors$importedNoButton)
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


test_that("Test 5 output matches", {
  # Compare the output to EpiEstim's output
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Pennsylvania2009.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_SI.Sample(sample)

  epiEstimOut <- EstimateR(I, T.Start=2:26, T.End=8:32, SI.Sample=sample, method="SIFromSample", n2=100, seed=1, Std.Prior=6)

  compareOutputFromApp(appOut, epiEstimOut)
})


