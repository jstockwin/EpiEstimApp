context("Test Suite 4 (E2E) --> Endpoint 9.1")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)



# ---------------------------------------------------------------------------#
# Test 6 - Different init_pars                                               #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.1 (Test 6)")
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

  test_that("can walk through the app to endpoint state (Test 6)", {
      # Walk the app through to endpoint state with default inputs
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_own_button)
    clickNext(remDr) # Move to state 7.2
    waitForStateDisplayed(remDr, "7.2")
    click(remDr, pages$state7.2$selectors$si_from_raw_button)
    clickNext(remDr) # Move to state 8.2
    waitForStateDisplayed(remDr, "8.2")
    if (getAttribute(remDr, pages$state8.2$selectors$si_data_upload_input, "value") == "") {
      # Selenium gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.2$selectors$si_data_upload_input, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalData/RotavirusEcuador2011.csv")
      sendKeys(remDr, pages$state8.2$selectors$si_data_upload_input,
               path)
    }
    sendKeys(remDr, pages$state8.2$selectors$seed_input, "1")
    clickNext(remDr) # Move to state 9.1
    waitForStateDisplayed(remDr, "9.1")
    sendKeys(remDr, pages$state9.1$selectors$seed_input, "1")
    sendKeys(remDr, pages$state9.1$selectors$param1_input, "2") # <---
    sendKeys(remDr, pages$state9.1$selectors$param2_input, "1") # <---
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


test_that("Test 6 output matches", {
  # Compare the output to EpiEstim's output
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  si_data <- read.csv(paste(appDir, "datasets/SerialIntervalData/RotavirusEcuador2011.csv", sep="/"), header=FALSE)
  si_data <- EpiEstim:::process_si_data(si_data)

  epiEstimOut <- estimate_R(incid, si_data=si_data, method="si_from_data",
                           config=list(t_start=2:26, t_end=8:32,
                           si_parametric_distr="G", n1=500,
                           n2=100, seed=1, mcmc_control=list(burnin=3000, thin=10, seed=1, init_pars=c(2,1)))
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 7 - Different distribution (1)                                        #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.1 (Test 7)")
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

  test_that("can walk through the app to endpoint state (Test 7)", {
      # Walk the app through to endpoint state with default inputs
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_own_button)
    clickNext(remDr) # Move to state 7.2
    waitForStateDisplayed(remDr, "7.2")
    click(remDr, pages$state7.2$selectors$si_from_raw_button)
    clickNext(remDr) # Move to state 8.2
    waitForStateDisplayed(remDr, "8.2")
    if (getAttribute(remDr, pages$state8.2$selectors$si_data_upload_input, "value") == "") {
      # Selenium gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.2$selectors$si_data_upload_input, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalData/RotavirusEcuador2011.csv")
      sendKeys(remDr, pages$state8.2$selectors$si_data_upload_input,
               path)
    }
    sendKeys(remDr, pages$state8.2$selectors$seed_input, "1")
    clickNext(remDr) # Move to state 9.1
    waitForStateDisplayed(remDr, "9.1")
    click(remDr, pages$state9.1$selectors$distribution_option_2_input) # <--
    sendKeys(remDr, pages$state9.1$selectors$seed_input, "1")
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


test_that("Test 7 output matches", {
  # Compare the output to EpiEstim's output
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  si_data <- read.csv(paste(appDir, "datasets/SerialIntervalData/RotavirusEcuador2011.csv", sep="/"), header=FALSE)
  si_data <- EpiEstim:::process_si_data(si_data)

  epiEstimOut <- estimate_R(incid, si_data=si_data, method="si_from_data",
                           config=list(t_start=2:26, t_end=8:32,
                           si_parametric_distr="off1G", n1=500,
                           n2=100, seed=1, mcmc_control=list(burnin=3000, thin=10, seed=1))
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 8 - Different distribution (1)                                        #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.1 (Test 8)")
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

  test_that("can walk through the app to endpoint state (Test 8)", {
      # Walk the app through to endpoint state with default inputs
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_own_button)
    clickNext(remDr) # Move to state 7.2
    waitForStateDisplayed(remDr, "7.2")
    click(remDr, pages$state7.2$selectors$si_from_raw_button)
    clickNext(remDr) # Move to state 8.2
    waitForStateDisplayed(remDr, "8.2")
    if (getAttribute(remDr, pages$state8.2$selectors$si_data_upload_input, "value") == "") {
      # Selenium gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.2$selectors$si_data_upload_input, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalData/RotavirusEcuador2011.csv")
      sendKeys(remDr, pages$state8.2$selectors$si_data_upload_input,
               path)
    }
    sendKeys(remDr, pages$state8.2$selectors$seed_input, "1")
    clickNext(remDr) # Move to state 9.1
    waitForStateDisplayed(remDr, "9.1")
    click(remDr, pages$state9.1$selectors$distribution_option_3_input) # <--
    sendKeys(remDr, pages$state9.1$selectors$seed_input, "1")
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


test_that("Test 8 output matches", {
  # Compare the output to EpiEstim's output
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  si_data <- read.csv(paste(appDir, "datasets/SerialIntervalData/RotavirusEcuador2011.csv", sep="/"), header=FALSE)
  si_data <- EpiEstim:::process_si_data(si_data)

  epiEstimOut <- estimate_R(incid, si_data=si_data, method="si_from_data",
                           config=list(t_start=2:26, t_end=8:32,
                           si_parametric_distr="W", n1=500,
                           n2=100, seed=1, mcmc_control=list(burnin=3000, thin=10, seed=1))
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 9 - Different distribution (1)                                        #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.1 (Test 9)")
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

  test_that("can walk through the app to endpoint state (Test 9)", {
      # Walk the app through to endpoint state with default inputs
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_own_button)
    clickNext(remDr) # Move to state 7.2
    waitForStateDisplayed(remDr, "7.2")
    click(remDr, pages$state7.2$selectors$si_from_raw_button)
    clickNext(remDr) # Move to state 8.2
    waitForStateDisplayed(remDr, "8.2")
    if (getAttribute(remDr, pages$state8.2$selectors$si_data_upload_input, "value") == "") {
      # Selenium gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.2$selectors$si_data_upload_input, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalData/RotavirusEcuador2011.csv")
      sendKeys(remDr, pages$state8.2$selectors$si_data_upload_input,
               path)
    }
    sendKeys(remDr, pages$state8.2$selectors$seed_input, "1")
    clickNext(remDr) # Move to state 9.1
    waitForStateDisplayed(remDr, "9.1")
    click(remDr, pages$state9.1$selectors$distribution_option_4_input) # <--
    sendKeys(remDr, pages$state9.1$selectors$seed_input, "1")
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


test_that("Test 9 output matches", {
  # Compare the output to EpiEstim's output
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  si_data <- read.csv(paste(appDir, "datasets/SerialIntervalData/RotavirusEcuador2011.csv", sep="/"), header=FALSE)
  si_data <- EpiEstim:::process_si_data(si_data)

  epiEstimOut <- estimate_R(incid, si_data=si_data, method="si_from_data",
                           config=list(t_start=2:26, t_end=8:32,
                           si_parametric_distr="off1W", n1=500,
                           n2=100, seed=1, mcmc_control=list(burnin=3000, thin=10, seed=1))
  )

  compareOutputFromApp(appOut, epiEstimOut)
})




# ---------------------------------------------------------------------------#
# Test 10 - Different distribution (1)                                        #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Endpoint 9.1 (Test 10)")
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

  test_that("can walk through the app to endpoint state (Test 10)", {
      # Walk the app through to endpoint state with default inputs
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_own_button)
    clickNext(remDr) # Move to state 7.2
    waitForStateDisplayed(remDr, "7.2")
    click(remDr, pages$state7.2$selectors$si_from_raw_button)
    clickNext(remDr) # Move to state 8.2
    waitForStateDisplayed(remDr, "8.2")
    if (getAttribute(remDr, pages$state8.2$selectors$si_data_upload_input, "value") == "") {
      # Selenium gives an error about interacting with an element
      # which is not currently visible. Explicitly show the element
      # first to fix this?
      setAttribute(remDr, pages$state8.2$selectors$si_data_upload_input, "style", "display: block;")
      path <- getFilePath(remDr, "datasets/SerialIntervalData/RotavirusEcuador2011.csv")
      sendKeys(remDr, pages$state8.2$selectors$si_data_upload_input,
               path)
    }
    sendKeys(remDr, pages$state8.2$selectors$seed_input, "1")
    clickNext(remDr) # Move to state 9.1
    waitForStateDisplayed(remDr, "9.1")
    click(remDr, pages$state9.1$selectors$distribution_option_6_input) # <--
    sendKeys(remDr, pages$state9.1$selectors$seed_input, "1")
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


test_that("Test 10 output matches", {
  # Compare the output to EpiEstim's output
  incid <- read.csv(paste(appDir, "datasets/IncidenceData/H1N1Pennsylvania2009.csv", sep="/"), header=FALSE)
  incid <- EpiEstim:::process_I(incid)
  si_data <- read.csv(paste(appDir, "datasets/SerialIntervalData/RotavirusEcuador2011.csv", sep="/"), header=FALSE)
  si_data <- EpiEstim:::process_si_data(si_data)

  epiEstimOut <- estimate_R(incid, si_data=si_data, method="si_from_data",
                           config=list(t_start=2:26, t_end=8:32,
                           si_parametric_distr="off1L", n1=500,
                           n2=100, seed=1, mcmc_control=list(burnin=3000, thin=10, seed=1))
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



