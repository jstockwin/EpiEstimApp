context("Test Suite 4 (E2E) --> Endpoint 9.1")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 1 - Different n1                                                      #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.1 (Test 1)")
rD <- drivers$rDr
remDr <- drivers$remDr

appOut <- NULL
openRemDriver(remDr)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr, timeout=3000) # Long timeout for running MCMC
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
    click(remDr, pages$state7.2$selectors$SIFromRawButton)
    clickNext(remDr) # Move to state 8.2
    waitForStateDisplayed(remDr, "8.2")
    if (getAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalData/RotavirusEcuador2011.csv")
      sendKeys(remDr, pages$state8.2$selectors$SIDataUploadInput,
               path)
    }
    sendKeys(remDr, pages$state8.2$selectors$seedInput, "1")
    clickNext(remDr) # Move to state 9.1
    waitForStateDisplayed(remDr, "9.1")
    sendKeys(remDr, pages$state9.1$selectors$seedInput, "1")
    clear(remDr, pages$state9.1$selectors$n1Input)           # <---
    sendKeys(remDr, pages$state9.1$selectors$n1Input, "400") # <---
    clickGo(remDr)
    Sys.sleep(1)
    waitForAppReady(remDr, timeout=3000) # Long timeout for running MCMC

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
  si_data <- read.csv(paste(appDir, "datasets/SerialIntervalData/RotavirusEcuador2011.csv", sep="/"), header=FALSE)
  si_data <- EpiEstim:::process_si_data(si_data)

  epiEstimOut <- EstimateR(I, t_start=2:26, t_end=8:32, si_data=si_data,
                           si_parametric_distr="G", method="si_from_data", n1=400,
                           n2=100, seed=1, mcmc_control=list(burnin=3000, thin=10, seed=1))

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 2 - Different burnin                                                  #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.1 (Test 2)")
rD <- drivers$rDr
remDr <- drivers$remDr

appOut <- NULL
openRemDriver(remDr)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr, timeout=3000) # Long timeout for running MCMC
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
    click(remDr, pages$state7.2$selectors$SIFromRawButton)
    clickNext(remDr) # Move to state 8.2
    waitForStateDisplayed(remDr, "8.2")
    if (getAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalData/RotavirusEcuador2011.csv")
      sendKeys(remDr, pages$state8.2$selectors$SIDataUploadInput,
               path)
    }
    sendKeys(remDr, pages$state8.2$selectors$seedInput, "1")
    clickNext(remDr) # Move to state 9.1
    waitForStateDisplayed(remDr, "9.1")
    sendKeys(remDr, pages$state9.1$selectors$seedInput, "1")
    clear(remDr, pages$state9.1$selectors$burninInput)           # <---
    sendKeys(remDr, pages$state9.1$selectors$burninInput, "3500") # <---
    clickGo(remDr)
    Sys.sleep(1)
    waitForAppReady(remDr, timeout=3000) # Long timeout for running MCMC

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
  si_data <- read.csv(paste(appDir, "datasets/SerialIntervalData/RotavirusEcuador2011.csv", sep="/"), header=FALSE)
  si_data <- EpiEstim:::process_si_data(si_data)

  epiEstimOut <- EstimateR(I, t_start=2:26, t_end=8:32, si_data=si_data,
                           si_parametric_distr="G", method="si_from_data", n1=500,
                           n2=100, seed=1, mcmc_control=list(burnin=3500, thin=10, seed=1))

  compareOutputFromApp(appOut, epiEstimOut)
})




# ---------------------------------------------------------------------------#
# Test 3 - Different thin                                                    #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.1 (Test 3)")
rD <- drivers$rDr
remDr <- drivers$remDr

appOut <- NULL
openRemDriver(remDr)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr, timeout=3000) # Long timeout for running MCMC
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
    click(remDr, pages$state7.2$selectors$SIFromRawButton)
    clickNext(remDr) # Move to state 8.2
    waitForStateDisplayed(remDr, "8.2")
    if (getAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalData/RotavirusEcuador2011.csv")
      sendKeys(remDr, pages$state8.2$selectors$SIDataUploadInput,
               path)
    }
    sendKeys(remDr, pages$state8.2$selectors$seedInput, "1")
    clickNext(remDr) # Move to state 9.1
    waitForStateDisplayed(remDr, "9.1")
    sendKeys(remDr, pages$state9.1$selectors$seedInput, "1")
    clear(remDr, pages$state9.1$selectors$thinInput)           # <---
    sendKeys(remDr, pages$state9.1$selectors$thinInput, "15") # <---
    clickGo(remDr)
    Sys.sleep(1)
    waitForAppReady(remDr, timeout=3000) # Long timeout for running MCMC

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
  si_data <- read.csv(paste(appDir, "datasets/SerialIntervalData/RotavirusEcuador2011.csv", sep="/"), header=FALSE)
  si_data <- EpiEstim:::process_si_data(si_data)

  epiEstimOut <- EstimateR(I, t_start=2:26, t_end=8:32, si_data=si_data,
                           si_parametric_distr="G", method="si_from_data", n1=500,
                           n2=100, seed=1, mcmc_control=list(burnin=3000, thin=15, seed=1))

  compareOutputFromApp(appOut, epiEstimOut)
})




# ---------------------------------------------------------------------------#
# Test 4 - Different n2                                                      #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.1 (Test 4)")
rD <- drivers$rDr
remDr <- drivers$remDr

appOut <- NULL
openRemDriver(remDr)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr, timeout=3000) # Long timeout for running MCMC
  })

  test_that("can walk through the app to endpoint state (Test 4)", {
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
    click(remDr, pages$state7.2$selectors$SIFromRawButton)
    clickNext(remDr) # Move to state 8.2
    waitForStateDisplayed(remDr, "8.2")
    if (getAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalData/RotavirusEcuador2011.csv")
      sendKeys(remDr, pages$state8.2$selectors$SIDataUploadInput,
               path)
    }
    sendKeys(remDr, pages$state8.2$selectors$seedInput, "1")
    clickNext(remDr) # Move to state 9.1
    waitForStateDisplayed(remDr, "9.1")
    sendKeys(remDr, pages$state9.1$selectors$seedInput, "1")
    clear(remDr, pages$state9.1$selectors$n2Input)           # <---
    sendKeys(remDr, pages$state9.1$selectors$n2Input, "150") # <---
    clickGo(remDr)
    Sys.sleep(1)
    waitForAppReady(remDr, timeout=3000) # Long timeout for running MCMC

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
  si_data <- read.csv(paste(appDir, "datasets/SerialIntervalData/RotavirusEcuador2011.csv", sep="/"), header=FALSE)
  si_data <- EpiEstim:::process_si_data(si_data)

  epiEstimOut <- EstimateR(I, t_start=2:26, t_end=8:32, si_data=si_data,
                           si_parametric_distr="G", method="si_from_data", n1=500,
                           n2=150, seed=1, mcmc_control=list(burnin=3000, thin=10, seed=1))

  compareOutputFromApp(appOut, epiEstimOut)
})




# ---------------------------------------------------------------------------#
# Test 5 - Different MCMC seed                                               #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.1 (Test 5)")
rD <- drivers$rDr
remDr <- drivers$remDr

appOut <- NULL
openRemDriver(remDr)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr, timeout=3000) # Long timeout for running MCMC
  })

  test_that("can walk through the app to endpoint state (Test 5)", {
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
    click(remDr, pages$state7.2$selectors$SIFromRawButton)
    clickNext(remDr) # Move to state 8.2
    waitForStateDisplayed(remDr, "8.2")
    if (getAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "value") == "") {
      # SAUCELABS gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.2$selectors$SIDataUploadInput, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalData/RotavirusEcuador2011.csv")
      sendKeys(remDr, pages$state8.2$selectors$SIDataUploadInput,
               path)
    }
    sendKeys(remDr, pages$state8.2$selectors$seedInput, "1")
    clickNext(remDr) # Move to state 9.1
    waitForStateDisplayed(remDr, "9.1")
    sendKeys(remDr, pages$state9.1$selectors$seedInput, "2") # <---
    clickGo(remDr)
    Sys.sleep(1)
    waitForAppReady(remDr, timeout=3000) # Long timeout for running MCMC

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
  si_data <- read.csv(paste(appDir, "datasets/SerialIntervalData/RotavirusEcuador2011.csv", sep="/"), header=FALSE)
  si_data <- EpiEstim:::process_si_data(si_data)

  epiEstimOut <- EstimateR(I, t_start=2:26, t_end=8:32, si_data=si_data,
                           si_parametric_distr="G", method="si_from_data", n1=500,
                           n2=100, seed=1, mcmc_control=list(burnin=3000, thin=10, seed=2))

  compareOutputFromApp(appOut, epiEstimOut)
})


