context("Test Suite 4 (E2E) --> Endpoint 7.5")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 1 - Defaults                                                          #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.5 (Test 1)")
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
    click(remDr, pages$state5.1$selectors$exposureDataNoInput)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$SIEstTypeOption3Button)
    clickNext(remDr) # Move to state 7.5
    waitForStateDisplayed(remDr, "7.5")
    if (getAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalDistributions/Flu1918.csv")
      sendKeys(remDr, pages$state7.5$selectors$SIDistrDataUploadInput,
               path)
    }
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
  I <- read.csv(paste(appDir, "datasets/IncidenceData/PennsylvaniaH1N1.csv", sep="/"), header=FALSE)
  I <- EpiEstim:::process_I(I)
  data(Flu1918)
  SI.Distr <- Flu1918$SI.Distr
  epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="NonParametricSI",
                           SI.Distr=SI.Distr)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 2 - Different dataset (1)                                             #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.5 (Test 2)")
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
    click(remDr, pages$state5.1$selectors$exposureDataNoInput)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$SIEstTypeOption3Button)
    clickNext(remDr) # Move to state 7.5
    waitForStateDisplayed(remDr, "7.5")
    if (getAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalDistributions/Flu2009.csv")
      sendKeys(remDr, pages$state7.5$selectors$SIDistrDataUploadInput,
               path)
    }
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
  I <- read.csv(paste(appDir, "datasets/IncidenceData/PennsylvaniaH1N1.csv", sep="/"), header=FALSE)
  I <- EpiEstim:::process_I(I)
  data(Flu2009)
  SI.Distr <- Flu2009$SI.Distr
  epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="NonParametricSI",
                           SI.Distr=SI.Distr)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 3 - Different dataset (2)                                             #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.5 (Test 3)")
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
    click(remDr, pages$state5.1$selectors$exposureDataNoInput)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$SIEstTypeOption3Button)
    clickNext(remDr) # Move to state 7.5
    waitForStateDisplayed(remDr, "7.5")
    if (getAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalDistributions/Measles1861.csv")
      sendKeys(remDr, pages$state7.5$selectors$SIDistrDataUploadInput,
               path)
    }
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
  I <- read.csv(paste(appDir, "datasets/IncidenceData/PennsylvaniaH1N1.csv", sep="/"), header=FALSE)
  I <- EpiEstim:::process_I(I)
  data(Measles1861)
  SI.Distr <- Measles1861$SI.Distr
  epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="NonParametricSI",
                           SI.Distr=SI.Distr)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 4 - Different dataset (3)                                             #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.5 (Test 4)")
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
    click(remDr, pages$state1.1$selectors$preloadedDataButton)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$datasetOption1Input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposureDataNoInput)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$SIEstTypeOption3Button)
    clickNext(remDr) # Move to state 7.5
    waitForStateDisplayed(remDr, "7.5")
    if (getAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalDistributions/SARS2003.csv")
      sendKeys(remDr, pages$state7.5$selectors$SIDistrDataUploadInput,
               path)
    }
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
  I <- read.csv(paste(appDir, "datasets/IncidenceData/PennsylvaniaH1N1.csv", sep="/"), header=FALSE)
  I <- EpiEstim:::process_I(I)
  data(SARS2003)
  SI.Distr <- SARS2003$SI.Distr
  epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="NonParametricSI",
                           SI.Distr=SI.Distr)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 5 - Different dataset (4)                                             #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.5 (Test 5)")
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
    click(remDr, pages$state1.1$selectors$preloadedDataButton)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$datasetOption1Input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposureDataNoInput)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$SIEstTypeOption3Button)
    clickNext(remDr) # Move to state 7.5
    waitForStateDisplayed(remDr, "7.5")
    if (getAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state7.5$selectors$SIDistrDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalDistributions/Smallpox17.5.csv")
      sendKeys(remDr, pages$state7.5$selectors$SIDistrDataUploadInput,
               path)
    }
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
  I <- read.csv(paste(appDir, "datasets/IncidenceData/PennsylvaniaH1N1.csv", sep="/"), header=FALSE)
  I <- EpiEstim:::process_I(I)
  data(Smallpox17.5)
  SI.Distr <- Smallpox17.5$SI.Distr
  epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="NonParametricSI",
                           SI.Distr=SI.Distr)

  compareOutputFromApp(appOut, epiEstimOut)
})


