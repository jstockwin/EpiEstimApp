context("Test Suite 4 (E2E) --> Endpoint 7.4")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 1 - Defaults                                                          #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.4 (Test 1)")
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_2_button)
    clickNext(remDr) # Move to state 7.4
    waitForStateDisplayed(remDr, "7.4")
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
  epiEstimOut <- EstimateR(I, method="parametric_si",
                           config=list(t_start=2:26, t_end=8:32, mean_si=2, std_si=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})




# ---------------------------------------------------------------------------#
# Test 2 - Different mean_si                                                 #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.4 (Test 2)")
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_2_button)
    clickNext(remDr) # Move to state 7.4
    waitForStateDisplayed(remDr, "7.4")
    clear(remDr, pages$state7.4$selectors$mean_si_input)
    sendKeys(remDr, pages$state7.4$selectors$mean_si_input, "3")
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
  epiEstimOut <- EstimateR(I, method="parametric_si",
                           config=list(t_start=2:26, t_end=8:32, mean_si=3, std_si=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})





# ---------------------------------------------------------------------------#
# Test 3 - Different std_si                                                  #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 7.4 (Test 3)")
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_2_button)
    clickNext(remDr) # Move to state 7.4
    waitForStateDisplayed(remDr, "7.4")
    clear(remDr, pages$state7.4$selectors$std_si_input)
    sendKeys(remDr, pages$state7.4$selectors$std_si_input, "2")
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
  epiEstimOut <- EstimateR(I, method="parametric_si",
                           config=list(t_start=2:26, t_end=8:32, mean_si=2, std_si=2)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})




