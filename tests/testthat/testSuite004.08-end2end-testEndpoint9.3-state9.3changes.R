context("Test Suite 4 (E2E) --> Endpoint 9.3")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 1 - Defaults                                                          #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.3 (Test 1)")
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
    click(remDr, pages$state6.2$selectors$uncertaintyNoButton)
    clickNext(remDr) # Move to state 7.4
    waitForStateDisplayed(remDr, "7.4")
    click(remDr, pages$state7.4$selectors$parametricNoButton)
    clickNext(remDr) # Move to state 8.5
    waitForStateDisplayed(remDr, "8.5")
    click(remDr, pages$state8.5$selectors$SIDistrDataTypePreloadedButton)
    clickNext(remDr) # Move to state 9.3
    waitForStateDisplayed(remDr, "9.3")
    click(remDr, pages$state9.3$selectors$datasetOption1Input)
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
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.3 (Test 2)")
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
    click(remDr, pages$state6.2$selectors$uncertaintyNoButton)
    clickNext(remDr) # Move to state 7.4
    waitForStateDisplayed(remDr, "7.4")
    click(remDr, pages$state7.4$selectors$parametricNoButton)
    clickNext(remDr) # Move to state 8.5
    waitForStateDisplayed(remDr, "8.5")
    click(remDr, pages$state8.5$selectors$SIDistrDataTypePreloadedButton)
    clickNext(remDr) # Move to state 9.3
    waitForStateDisplayed(remDr, "9.3")
    click(remDr, pages$state9.3$selectors$datasetOption2Input)
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
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.3 (Test 3)")
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
    click(remDr, pages$state6.2$selectors$uncertaintyNoButton)
    clickNext(remDr) # Move to state 7.4
    waitForStateDisplayed(remDr, "7.4")
    click(remDr, pages$state7.4$selectors$parametricNoButton)
    clickNext(remDr) # Move to state 8.5
    waitForStateDisplayed(remDr, "8.5")
    click(remDr, pages$state8.5$selectors$SIDistrDataTypePreloadedButton)
    clickNext(remDr) # Move to state 9.3
    waitForStateDisplayed(remDr, "9.3")
    click(remDr, pages$state9.3$selectors$datasetOption3Input)
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
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.3 (Test 4)")
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
    click(remDr, pages$state6.2$selectors$uncertaintyNoButton)
    clickNext(remDr) # Move to state 7.4
    waitForStateDisplayed(remDr, "7.4")
    click(remDr, pages$state7.4$selectors$parametricNoButton)
    clickNext(remDr) # Move to state 8.5
    waitForStateDisplayed(remDr, "8.5")
    click(remDr, pages$state8.5$selectors$SIDistrDataTypePreloadedButton)
    clickNext(remDr) # Move to state 9.3
    waitForStateDisplayed(remDr, "9.3")
    click(remDr, pages$state9.3$selectors$datasetOption4Input)
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
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.3 (Test 5)")
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
    click(remDr, pages$state6.2$selectors$uncertaintyNoButton)
    clickNext(remDr) # Move to state 7.4
    waitForStateDisplayed(remDr, "7.4")
    click(remDr, pages$state7.4$selectors$parametricNoButton)
    clickNext(remDr) # Move to state 8.5
    waitForStateDisplayed(remDr, "8.5")
    click(remDr, pages$state8.5$selectors$SIDistrDataTypePreloadedButton)
    clickNext(remDr) # Move to state 9.3
    waitForStateDisplayed(remDr, "9.3")
    click(remDr, pages$state9.3$selectors$datasetOption5Input)
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
  data(Smallpox1972)
  SI.Distr <- Smallpox1972$SI.Distr
  epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="NonParametricSI",
                           SI.Distr=SI.Distr)

  compareOutputFromApp(appOut, epiEstimOut)
})


