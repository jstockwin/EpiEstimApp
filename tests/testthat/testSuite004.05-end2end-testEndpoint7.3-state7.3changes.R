context("Test Suite 4 (E2E) --> Endpoint 7.3")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 1 - Defaults                                                          #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 1)")
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
    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
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
epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=50,
                         n2=50, Mean.SI=2, Std.Mean.SI=1, Min.Mean.SI=1, Max.Mean.SI=3,
                         Std.SI=2, Std.Std.SI=1, Min.Std.SI=1, Max.Std.SI=3, seed=1)

test_that("Test 1 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})

# ---------------------------------------------------------------------------#
# Test 2 - Different n1                                                      #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 2)")
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
    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
    clear(remDr, pages$state7.3$selectors$n1Input) # <---
    sendKeys(remDr, pages$state7.3$selectors$n1Input, "60") # <---
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
epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=60,
                         n2=50, Mean.SI=2, Std.Mean.SI=1, Min.Mean.SI=1, Max.Mean.SI=3,
                         Std.SI=2, Std.Std.SI=1, Min.Std.SI=1, Max.Std.SI=3, seed=1)

test_that("Test 2 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 3 - Different n2                                                      #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 3)")
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
    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
    clear(remDr, pages$state7.3$selectors$n2Input) # <---
    sendKeys(remDr, pages$state7.3$selectors$n2Input, "60") # <---
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
epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=50,
                         n2=60, Mean.SI=2, Std.Mean.SI=1, Min.Mean.SI=1, Max.Mean.SI=3,
                         Std.SI=2, Std.Std.SI=1, Min.Std.SI=1, Max.Std.SI=3, seed=1)

test_that("Test 3 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 4 - Different Mean.SI                                                 #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 4)")
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
    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
    clear(remDr, pages$state7.3$selectors$Mean.SIInput) # <---
    sendKeys(remDr, pages$state7.3$selectors$Mean.SIInput, "2.5") # <--
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
epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=50,
                         n2=50, Mean.SI=2.5, Std.Mean.SI=1, Min.Mean.SI=1, Max.Mean.SI=3,
                         Std.SI=2, Std.Std.SI=1, Min.Std.SI=1, Max.Std.SI=3, seed=1)

test_that("Test 4 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 5 - Different Std.Mean.SI                                             #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 5)")
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
    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
    clear(remDr, pages$state7.3$selectors$Std.Mean.SIInput) # <---
    sendKeys(remDr, pages$state7.3$selectors$Std.Mean.SIInput, "2") # <--
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
epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=50,
                         n2=50, Mean.SI=2, Std.Mean.SI=2, Min.Mean.SI=1, Max.Mean.SI=3,
                         Std.SI=2, Std.Std.SI=1, Min.Std.SI=1, Max.Std.SI=3, seed=1)

test_that("Test 5 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 6 - Different Min.Mean.SI                                             #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 6)")
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
    click(remDr, pages$state5.1$selectors$exposureDataNoInput)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
    clear(remDr, pages$state7.3$selectors$Min.Mean.SIInput) # <---
    sendKeys(remDr, pages$state7.3$selectors$Min.Mean.SIInput, "1.5") # <--
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
epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=50,
                         n2=50, Mean.SI=2, Std.Mean.SI=1, Min.Mean.SI=1.5, Max.Mean.SI=3,
                         Std.SI=2, Std.Std.SI=1, Min.Std.SI=1, Max.Std.SI=3, seed=1)

test_that("Test 6 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 7 - Different Max.Mean.SI                                             #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 7)")
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
    click(remDr, pages$state5.1$selectors$exposureDataNoInput)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
    clear(remDr, pages$state7.3$selectors$Max.Mean.SIInput) # <---
    sendKeys(remDr, pages$state7.3$selectors$Max.Mean.SIInput, "4") # <--
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
epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=50,
                         n2=50, Mean.SI=2, Std.Mean.SI=1, Min.Mean.SI=1, Max.Mean.SI=4,
                         Std.SI=2, Std.Std.SI=1, Min.Std.SI=1, Max.Std.SI=3, seed=1)

test_that("Test 7 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 8 - Different Std.SI                                                  #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 8)")
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

  test_that("can walk through the app to endpoint state (Test 8)", {
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
    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
    clear(remDr, pages$state7.3$selectors$Std.SIInput) # <---
    sendKeys(remDr, pages$state7.3$selectors$Std.SIInput, "2.5") # <--
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
epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=50,
                         n2=50, Mean.SI=2, Std.Mean.SI=1, Min.Mean.SI=1, Max.Mean.SI=3,
                         Std.SI=2.5, Std.Std.SI=1, Min.Std.SI=1, Max.Std.SI=3, seed=1)

test_that("Test 8 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 9 - Different Std.Std.SI                                              #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 9)")
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

  test_that("can walk through the app to endpoint state (Test 9)", {
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
    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
    clear(remDr, pages$state7.3$selectors$Std.Std.SIInput) # <---
    sendKeys(remDr, pages$state7.3$selectors$Std.Std.SIInput, "2") # <--
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
epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=50,
                         n2=50, Mean.SI=2, Std.Mean.SI=1, Min.Mean.SI=1, Max.Mean.SI=3,
                         Std.SI=2, Std.Std.SI=2, Min.Std.SI=1, Max.Std.SI=3, seed=1)

test_that("Test 9 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 10 - Different Min.Std.SI                                              #
# ---------------------------------------------------------------------------#

# Test commented out ref https://github.com/jstockwin/EpiEstimApp/issues/126

#drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 10)")
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
#  test_that("can walk through the app to endpoint state (Test 10)", {
#      # Walk the app through to endpoint state with default inputs
#    click(remDr, pages$state1.1$selectors$preloadedDataButton)
#    clickNext(remDr) # Move to state 2.2
#    waitForStateDisplayed(remDr, "2.2")
#    click(remDr, pages$state2.2$selectors$datasetOption1Input)
#    clickNext(remDr) # Move to state 5.1
#    waitForStateDisplayed(remDr, "5.1")
#    click(remDr, pages$state5.1$selectors$exposureDataNoInput)
#    clickNext(remDr) # Move to state 6.2
#    waitForStateDisplayed(remDr, "6.2")
#    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
#    clickNext(remDr) # Move to state 7.3
#    waitForStateDisplayed(remDr, "7.3")
#    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
#    clear(remDr, pages$state7.3$selectors$Min.Std.SIInput) # <---
#    sendKeys(remDr, pages$state7.3$selectors$Min.Std.SIInput, "1.5") # <--
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
#I <- read.csv(paste(appDir, "datasets/IncidenceData/PennsylvaniaH1N1.csv", sep="/"), header=FALSE)
#I <- EpiEstim:::process_I(I)
#epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=50,
#                         n2=50, Mean.SI=2, Std.Mean.SI=1, Min.Mean.SI=1, Max.Mean.SI=3,
#                         Std.SI=2, Std.Std.SI=1, Min.Std.SI=1.5, Max.Std.SI=3, seed=1)
#
#test_that("Test 10 output matches", {
#  compareOutputFromApp(appOut, epiEstimOut)
#})



# ---------------------------------------------------------------------------#
# Test 11 - Different Max.Std.SI                                              #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 11)")
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

  test_that("can walk through the app to endpoint state (Test 11)", {
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
    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
    clear(remDr, pages$state7.3$selectors$Max.Std.SIInput) # <---
    sendKeys(remDr, pages$state7.3$selectors$Max.Std.SIInput, "4") # <--
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
epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=50,
                         n2=50, Mean.SI=2, Std.Mean.SI=1, Min.Mean.SI=1, Max.Mean.SI=3,
                         Std.SI=2, Std.Std.SI=1, Min.Std.SI=1, Max.Std.SI=4, seed=1)

test_that("Test 11 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 12 - Different Seed                                                   #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.3 (Test 12)")
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

  test_that("can walk through the app to endpoint state (Test 12)", {
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
    click(remDr, pages$state6.2$selectors$uncertaintyYesButton)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "1")
    clear(remDr, pages$state7.3$selectors$seedInput) # <---
    sendKeys(remDr, pages$state7.3$selectors$seedInput, "2") # <--
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
epiEstimOut <- EstimateR(I, T.Start=2:25, T.End=8:31, method="UncertainSI", n1=50,
                         n2=50, Mean.SI=2, Std.Mean.SI=1, Min.Mean.SI=1, Max.Mean.SI=3,
                         Std.SI=2, Std.Std.SI=1, Min.Std.SI=1, Max.Std.SI=3, seed=2)

test_that("Test 12 output matches", {
  compareOutputFromApp(appOut, epiEstimOut)
})



