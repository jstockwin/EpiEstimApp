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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
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
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  epiEstimOut <- estimate_r(incid, method="uncertain_si",
                           config=list(t_start=2:26, t_end=8:32, n1=50,
                           n2=50, mean_si=2, std_mean_si=1, min_mean_si=1, max_mean_si=3,
                           std_si=2, std_std_si=1, min_std_si=1, max_std_si=3, seed=1)
  )

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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
    clear(remDr, pages$state7.3$selectors$n1_input) # <---
    sendKeys(remDr, pages$state7.3$selectors$n1_input, "60") # <---
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
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  epiEstimOut <- estimate_r(incid, method="uncertain_si",
                           config=list(t_start=2:26, t_end=8:32, n1=60,
                           n2=50, mean_si=2, std_mean_si=1, min_mean_si=1, max_mean_si=3,
                           std_si=2, std_std_si=1, min_std_si=1, max_std_si=3, seed=1)
  )

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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
    clear(remDr, pages$state7.3$selectors$n2_input) # <---
    sendKeys(remDr, pages$state7.3$selectors$n2_input, "60") # <---
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
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  epiEstimOut <- estimate_r(incid, method="uncertain_si",
                           config=list(t_start=2:26, t_end=8:32, n1=50,
                           n2=60, mean_si=2, std_mean_si=1, min_mean_si=1, max_mean_si=3,
                           std_si=2, std_std_si=1, min_std_si=1, max_std_si=3, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 4 - Different mean_si                                                 #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
    clear(remDr, pages$state7.3$selectors$mean_si_input) # <---
    sendKeys(remDr, pages$state7.3$selectors$mean_si_input, "2.5") # <--
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
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  epiEstimOut <- estimate_r(incid, method="uncertain_si",
                           config=list(t_start=2:26, t_end=8:32, n1=50,
                           n2=50, mean_si=2.5, std_mean_si=1, min_mean_si=1, max_mean_si=3,
                           std_si=2, std_std_si=1, min_std_si=1, max_std_si=3, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 5 - Different std_mean_si                                             #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
    clear(remDr, pages$state7.3$selectors$std_mean_si_input) # <---
    sendKeys(remDr, pages$state7.3$selectors$std_mean_si_input, "2") # <--
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
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  epiEstimOut <- estimate_r(incid, method="uncertain_si",
                           config=list(t_start=2:26, t_end=8:32, n1=50,
                           n2=50, mean_si=2, std_mean_si=2, min_mean_si=1, max_mean_si=3,
                           std_si=2, std_std_si=1, min_std_si=1, max_std_si=3, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 6 - Different min_mean_si                                             #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
    clear(remDr, pages$state7.3$selectors$min_mean_si_input) # <---
    sendKeys(remDr, pages$state7.3$selectors$min_mean_si_input, "1.5") # <--
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
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  epiEstimOut <- estimate_r(incid, method="uncertain_si",
                           config=list(t_start=2:26, t_end=8:32, n1=50,
                           n2=50, mean_si=2, std_mean_si=1, min_mean_si=1.5, max_mean_si=3,
                           std_si=2, std_std_si=1, min_std_si=1, max_std_si=3, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 7 - Different max_mean_si                                             #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
    clear(remDr, pages$state7.3$selectors$max_mean_si_input) # <---
    sendKeys(remDr, pages$state7.3$selectors$max_mean_si_input, "4") # <--
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
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  epiEstimOut <- estimate_r(incid, method="uncertain_si",
                           config=list(t_start=2:26, t_end=8:32, n1=50,
                           n2=50, mean_si=2, std_mean_si=1, min_mean_si=1, max_mean_si=4,
                           std_si=2, std_std_si=1, min_std_si=1, max_std_si=3, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 8 - Different std_si                                                  #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
    clear(remDr, pages$state7.3$selectors$std_si_input) # <---
    sendKeys(remDr, pages$state7.3$selectors$std_si_input, "2.5") # <--
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


test_that("Test 8 output matches", {
  # Compare the output to EpiEstim's output
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  epiEstimOut <- estimate_r(incid, method="uncertain_si",
                           config=list(t_start=2:26, t_end=8:32, n1=50,
                           n2=50, mean_si=2, std_mean_si=1, min_mean_si=1, max_mean_si=3,
                           std_si=2.5, std_std_si=1, min_std_si=1, max_std_si=3, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 9 - Different std_std_si                                              #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
    clear(remDr, pages$state7.3$selectors$std_std_si_input) # <---
    sendKeys(remDr, pages$state7.3$selectors$std_std_si_input, "2") # <--
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


test_that("Test 9 output matches", {
  # Compare the output to EpiEstim's output
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  epiEstimOut <- estimate_r(incid, method="uncertain_si",
                           config=list(t_start=2:26, t_end=8:32, n1=50,
                           n2=50, mean_si=2, std_mean_si=1, min_mean_si=1, max_mean_si=3,
                           std_si=2, std_std_si=2, min_std_si=1, max_std_si=3, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 10 - Different min_std_si                                              #
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
#    click(remDr, pages$state1.1$selectors$preloaded_data_button)
#    clickNext(remDr) # Move to state 2.2
#    waitForStateDisplayed(remDr, "2.2")
#    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
#    clickNext(remDr) # Move to state 5.1
#    waitForStateDisplayed(remDr, "5.1")
#    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
#    clickNext(remDr) # Move to state 6.2
#    waitForStateDisplayed(remDr, "6.2")
#    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
#    clickNext(remDr) # Move to state 7.3
#    waitForStateDisplayed(remDr, "7.3")
#    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
#    clear(remDr, pages$state7.3$selectors$min_std_si_input) # <---
#    sendKeys(remDr, pages$state7.3$selectors$min_std_si_input, "1.5") # <--
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
#test_that("Test 10 output matches", {
## Compare the output to EpiEstim's output
#incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
#incid <- EpiEstim:::process_I(incid)
#epiEstimOut <- estimate_r(incid, t_start=2:26, t_end=8:32, method="uncertain_si", n1=50,
#                         n2=50, mean_si=2, std_mean_si=1, min_mean_si=1, max_mean_si=3,
#                         std_si=2, std_std_si=1, min_std_si=1.5, max_std_si=3, seed=1)
#
#  compareOutputFromApp(appOut, epiEstimOut)
#})



# ---------------------------------------------------------------------------#
# Test 11 - Different max_std_si                                              #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
    clear(remDr, pages$state7.3$selectors$max_std_si_input) # <---
    sendKeys(remDr, pages$state7.3$selectors$max_std_si_input, "4") # <--
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


test_that("Test 11 output matches", {
  # Compare the output to EpiEstim's output
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  epiEstimOut <- estimate_r(incid, method="uncertain_si",
                           config=list(t_start=2:26, t_end=8:32, n1=50,
                           n2=50, mean_si=2, std_mean_si=1, min_mean_si=1, max_mean_si=3,
                           std_si=2, std_std_si=1, min_std_si=1, max_std_si=4, seed=1)
  )

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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_no_input)
    clickNext(remDr) # Move to state 6.2
    waitForStateDisplayed(remDr, "6.2")
    click(remDr, pages$state6.2$selectors$si_est_type_option_1_button)
    clickNext(remDr) # Move to state 7.3
    waitForStateDisplayed(remDr, "7.3")
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "1")
    clear(remDr, pages$state7.3$selectors$seed_input) # <---
    sendKeys(remDr, pages$state7.3$selectors$seed_input, "2") # <--
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


test_that("Test 12 output matches", {
  # Compare the output to EpiEstim's output
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  epiEstimOut <- estimate_r(incid, method="uncertain_si",
                           config=list(t_start=2:26, t_end=8:32, n1=50,
                           n2=50, mean_si=2, std_mean_si=1, min_mean_si=1, max_mean_si=3,
                           std_si=2, std_std_si=1, min_std_si=1, max_std_si=3, seed=2)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



