context("Test Suite 4 (E2E) --> Incidence")

library(RSelenium)
library(testthat)
library(EpiEstim)
source("functions.R", local=TRUE)


# ---------------------------------------------------------------------------#
# Test 1 - Difference incidence data (1)                                     #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_2_input) # <---
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_preloaded_button)
    clickNext(remDr) # Move to state 7.1
    waitForStateDisplayed(remDr, "7.1")
    click(remDr, pages$state7.1$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 8.1
    waitForStateDisplayed(remDr, "8.1")
    click(remDr, pages$state8.1$selectors$distribution_option_1_input)
    sendKeys(remDr, pages$state8.1$selectors$seed_input, "1")
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
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1NewYork2009.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_si_sample(sample)

  epiEstimOut <- estimate_r(I, si_sample=sample, method="si_from_sample",
                           config=list(t_start=2:8, t_end=8:14, n2=100, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 2 - Difference incidence data (2)                                     #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_3_input) # <---
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_preloaded_button)
    clickNext(remDr) # Move to state 7.1
    waitForStateDisplayed(remDr, "7.1")
    click(remDr, pages$state7.1$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 8.1
    waitForStateDisplayed(remDr, "8.1")
    click(remDr, pages$state8.1$selectors$distribution_option_1_input)
    sendKeys(remDr, pages$state8.1$selectors$seed_input, "1")
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
  sample <- EpiEstim:::process_si_sample(sample)

  epiEstimOut <- estimate_r(I, si_sample=sample, method="si_from_sample",
                           config=list(t_start=2:17, t_end=8:23, n2=100, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 3 - Difference incidence data (3)                                     #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_4_input) # <---
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_preloaded_button)
    clickNext(remDr) # Move to state 7.1
    waitForStateDisplayed(remDr, "7.1")
    click(remDr, pages$state7.1$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 8.1
    waitForStateDisplayed(remDr, "8.1")
    click(remDr, pages$state8.1$selectors$distribution_option_1_input)
    sendKeys(remDr, pages$state8.1$selectors$seed_input, "1")
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
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Maryland1918.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_si_sample(sample)

  epiEstimOut <- estimate_r(I, si_sample=sample, method="si_from_sample",
                           config=list(t_start=2:86, t_end=8:92, n2=100, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 4 - Difference incidence data (4)                                     #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_5_input) # <---
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_preloaded_button)
    clickNext(remDr) # Move to state 7.1
    waitForStateDisplayed(remDr, "7.1")
    click(remDr, pages$state7.1$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 8.1
    waitForStateDisplayed(remDr, "8.1")
    click(remDr, pages$state8.1$selectors$distribution_option_1_input)
    sendKeys(remDr, pages$state8.1$selectors$seed_input, "1")
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
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/MeaslesGermany1861.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_si_sample(sample)

  epiEstimOut <- estimate_r(I, si_sample=sample, method="si_from_sample",
                           config=list(t_start=2:42, t_end=8:48, n2=100, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 5 - Difference incidence data (5)                                     #
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
    click(remDr, pages$state1.1$selectors$preloaded_data_button)
    clickNext(remDr) # Move to state 2.2
    waitForStateDisplayed(remDr, "2.2")
    click(remDr, pages$state2.2$selectors$dataset_option_6_input) # <---
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_preloaded_button)
    clickNext(remDr) # Move to state 7.1
    waitForStateDisplayed(remDr, "7.1")
    click(remDr, pages$state7.1$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 8.1
    waitForStateDisplayed(remDr, "8.1")
    click(remDr, pages$state8.1$selectors$distribution_option_1_input)
    sendKeys(remDr, pages$state8.1$selectors$seed_input, "1")
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
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/SARSHongKong2003.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_si_sample(sample)

  epiEstimOut <- estimate_r(I, si_sample=sample, method="si_from_sample",
                           config=list(t_start=2:101, t_end=8:107, n2=100, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})



# ---------------------------------------------------------------------------#
# Test 6 - Difference incidence data (6)                                     #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Incidence (Test 6)")
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
    click(remDr, pages$state2.2$selectors$dataset_option_7_input) # <---
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_preloaded_button)
    clickNext(remDr) # Move to state 7.1
    waitForStateDisplayed(remDr, "7.1")
    click(remDr, pages$state7.1$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 8.1
    waitForStateDisplayed(remDr, "8.1")
    click(remDr, pages$state8.1$selectors$distribution_option_1_input)
    sendKeys(remDr, pages$state8.1$selectors$seed_input, "1")
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
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/SmallpoxKosovo1972.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_si_sample(sample)

  epiEstimOut <- estimate_r(I, si_sample=sample, method="si_from_sample",
                           config=list(t_start=2:51, t_end=8:57, n2=100, seed=1)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 8 - Different mean_prior                                              #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Incidence (Test 8)")
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
    clear(remDr, pages$state2.2$selectors$mean_prior_input) # <---
    sendKeys(remDr, pages$state2.2$selectors$mean_prior_input, "6") # <---
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_preloaded_button)
    clickNext(remDr) # Move to state 7.1
    waitForStateDisplayed(remDr, "7.1")
    click(remDr, pages$state7.1$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 8.1
    waitForStateDisplayed(remDr, "8.1")
    click(remDr, pages$state8.1$selectors$distribution_option_1_input)
    sendKeys(remDr, pages$state8.1$selectors$seed_input, "1")
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
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Pennsylvania2009.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_si_sample(sample)

  epiEstimOut <- estimate_r(I, si_sample=sample, method="si_from_sample",
                           config=list(t_start=2:26, t_end=8:32, n2=100, seed=1, mean_prior=6)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})


# ---------------------------------------------------------------------------#
# Test 9 - Different std_prior                                              #
# ---------------------------------------------------------------------------#
drivers <- getRemDrivers("Test Suite 4 (E2E) --> Incidence (Test 9)")
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
    clear(remDr, pages$state2.2$selectors$std_prior_input) # <---
    sendKeys(remDr, pages$state2.2$selectors$std_prior_input, "6") # <---
    clickNext(remDr) # Move to state 5.1
    waitForStateDisplayed(remDr, "5.1")
    click(remDr, pages$state5.1$selectors$exposure_data_yes_input)
    clickNext(remDr) # Move to state 6.1
    waitForStateDisplayed(remDr, "6.1")
    click(remDr, pages$state6.1$selectors$si_data_type_preloaded_button)
    clickNext(remDr) # Move to state 7.1
    waitForStateDisplayed(remDr, "7.1")
    click(remDr, pages$state7.1$selectors$dataset_option_1_input)
    clickNext(remDr) # Move to state 8.1
    waitForStateDisplayed(remDr, "8.1")
    click(remDr, pages$state8.1$selectors$distribution_option_1_input)
    sendKeys(remDr, pages$state8.1$selectors$seed_input, "1")
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
  I <- read.csv(paste(appDir, 'datasets/IncidenceData/H1N1Pennsylvania2009.csv', sep='/'), header=FALSE)
  I <- EpiEstim:::process_I(I)
  sample <- read.csv(paste(appDir, "datasets/SIPosteriorSamples/RotavirusEcuador2011_SISamples_G.csv", sep="/"), header=FALSE)
  sample <- EpiEstim:::process_si_sample(sample)

  epiEstimOut <- estimate_r(I, si_sample=sample, method="si_from_sample",
                           config=list(t_start=2:26, t_end=8:32, n2=100, seed=1, std_prior=6)
  )

  compareOutputFromApp(appOut, epiEstimOut)
})


