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
    click(remDr, pages$state7.2$selectors$si_from_sample_button)
    clickNext(remDr) # Move to state 8.3
    waitForStateDisplayed(remDr, "8.3")
    path <- getFilePath(remDr, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv")
    sendKeys(remDr, pages$state8.3$selectors$si_sample_data_upload_input, path)
    waitForElemDisplayed(remDr, pages$state8.3$selectors$si_sample_data_upload_complete)
    sendKeys(remDr, pages$state8.3$selectors$seed_input, "1")
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
  si_sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  si_sample <- EpiEstim:::process_si_sample(si_sample)

  epiEstimOut <- estimate_R(incid, method="si_from_sample", si_sample=si_sample,
                           config=list(t_start=2:26, t_end=8:32, n2=100, seed=1)
  )

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
    click(remDr, pages$state7.2$selectors$si_from_sample_button)
    clickNext(remDr) # Move to state 8.3
    waitForStateDisplayed(remDr, "8.3")
    path <- getFilePath(remDr, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_W.csv")
    sendKeys(remDr, pages$state8.3$selectors$si_sample_data_upload_input, path)
    waitForElemDisplayed(remDr, pages$state8.3$selectors$si_sample_data_upload_complete)
    sendKeys(remDr, pages$state8.3$selectors$seed_input, "1")
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
  si_sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_W.csv", sep="/"), header=FALSE)
  si_sample <- EpiEstim:::process_si_sample(si_sample)

  epiEstimOut <- estimate_R(incid, method="si_from_sample", si_sample=si_sample,
                           config=list(t_start=2:26, t_end=8:32, n2=100, seed=1)
  )

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
    click(remDr, pages$state7.2$selectors$si_from_sample_button)
    clickNext(remDr) # Move to state 8.3
    waitForStateDisplayed(remDr, "8.3")
    path <- getFilePath(remDr, "datasets/SIPosteriorSamples/H1N1NewYork2009_SISamples_G.csv")
    sendKeys(remDr, pages$state8.3$selectors$si_sample_data_upload_input, path)
    waitForElemDisplayed(remDr, pages$state8.3$selectors$si_sample_data_upload_complete)
    sendKeys(remDr, pages$state8.3$selectors$seed_input, "1")
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
  si_sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/H1N1NewYork2009_SISamples_G.csv", sep="/"), header=FALSE)
  si_sample <- EpiEstim:::process_si_sample(si_sample)

  epiEstimOut <- estimate_R(incid, method="si_from_sample", si_sample=si_sample,
                           config=list(t_start=2:26, t_end=8:32,
                                       n2=100, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})
