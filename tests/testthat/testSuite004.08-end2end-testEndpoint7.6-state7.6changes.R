context("Test Suite 4 (E2E) --> Endpoint 7.6")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 1 - Defaults                                                          #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.6 (Test 1)")
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
    click(remDr, pages$state6.2$selectors$SIEstTypeOption4Button)
    clickNext(remDr) # Move to state 7.6
    waitForStateDisplayed(remDr, "7.6")
    click(remDr, pages$state7.6$selectors$datasetOption1Input)
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
  I <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  I <- EpiEstim:::process_I(I)
  SI.Distr <- as.numeric(read.csv(paste(appDir, "datasets/SerialIntervalDistributions/H1N1Maryland1918.csv", sep="/"), header=FALSE))
  epiEstimOut <- EstimateR(I, t_start=2:26, t_end=8:32, method="non_parametric_si",
                           SI.Distr=SI.Distr)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 2 - Different dataset (1)                                             #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.6 (Test 2)")
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
    click(remDr, pages$state6.2$selectors$SIEstTypeOption4Button)
    clickNext(remDr) # Move to state 7.6
    waitForStateDisplayed(remDr, "7.6")
    click(remDr, pages$state7.6$selectors$datasetOption2Input)
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
  I <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  I <- EpiEstim:::process_I(I)
  SI.Distr <- as.numeric(read.csv(paste(appDir, "datasets/SerialIntervalDistributions/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE))
  epiEstimOut <- EstimateR(I, t_start=2:26, t_end=8:32, method="non_parametric_si",
                           SI.Distr=SI.Distr)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 3 - Different dataset (2)                                             #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.6 (Test 3)")
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
    click(remDr, pages$state6.2$selectors$SIEstTypeOption4Button)
    clickNext(remDr) # Move to state 7.6
    waitForStateDisplayed(remDr, "7.6")
    click(remDr, pages$state7.6$selectors$datasetOption3Input)
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
  I <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  I <- EpiEstim:::process_I(I)
  SI.Distr <- as.numeric(read.csv(paste(appDir, "datasets/SerialIntervalDistributions/MeaslesGermany1861.csv", sep="/"), header=FALSE))
  epiEstimOut <- EstimateR(I, t_start=2:26, t_end=8:32, method="non_parametric_si",
                           SI.Distr=SI.Distr)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 4 - Different dataset (3)                                             #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.6 (Test 4)")
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
    click(remDr, pages$state6.2$selectors$SIEstTypeOption4Button)
    clickNext(remDr) # Move to state 7.6
    waitForStateDisplayed(remDr, "7.6")
    click(remDr, pages$state7.6$selectors$datasetOption4Input)
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
  I <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  I <- EpiEstim:::process_I(I)
  SI.Distr <- as.numeric(read.csv(paste(appDir, "datasets/SerialIntervalDistributions/SARSHongKong2003.csv", sep="/"), header=FALSE))
  epiEstimOut <- EstimateR(I, t_start=2:26, t_end=8:32, method="non_parametric_si",
                           SI.Distr=SI.Distr)

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 5 - Different dataset (4)                                             #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.6 (Test 5)")
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
    click(remDr, pages$state6.2$selectors$SIEstTypeOption4Button)
    clickNext(remDr) # Move to state 7.6
    waitForStateDisplayed(remDr, "7.6")
    click(remDr, pages$state7.6$selectors$datasetOption5Input)
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
  I <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  I <- EpiEstim:::process_I(I)
  SI.Distr <- as.numeric(read.csv(paste(appDir, "datasets/SerialIntervalDistributions/SmallpoxKosovo1972.csv", sep="/"), header=FALSE))
  epiEstimOut <- EstimateR(I, t_start=2:26, t_end=8:32, method="non_parametric_si",
                           SI.Distr=SI.Distr)

  compareOutputFromApp(appOut, epiEstimOut)
})


