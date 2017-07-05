context("Test Suite 2 (States) --> State 2.2")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 2.2")
rD <- drivers$rDr
remDr <- drivers$remDr

openRemDriver(remDr)
tryCatch({
  test_that("can connect to app", {
    connectToApp(remDr)
  })

  test_that("app is ready within 30 seconds", {
    waitForAppReady(remDr)
  })

  test_that("can navigate to state 2.2", {
   navigateToState(remDr, "2.2")
  })

  test_that("incidence dataset selection input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetLabel))
    expect_equal(getText(remDr, pages$state2.2$selectors$datasetLabel),
                 "Choose your dataset")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption1Label))
    expect_equal(getText(remDr, pages$state2.2$selectors$datasetOption1Label),
                 "FluPennsylvania2009")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption1Input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption2Label))
    expect_equal(getText(remDr, pages$state2.2$selectors$datasetOption2Label),
                 "FluNewYork2009")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption2Input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption3Label))
    expect_equal(getText(remDr, pages$state2.2$selectors$datasetOption3Label),
                 "RotavirusKiribati2013")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption3Input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption4Label))
    expect_equal(getText(remDr, pages$state2.2$selectors$datasetOption4Label),
                 "FluMaryland1918")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption4Input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption5Label))
    expect_equal(getText(remDr, pages$state2.2$selectors$datasetOption5Label),
                 "MeaslesGermany1861")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption5Input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption6Label))
    expect_equal(getText(remDr, pages$state2.2$selectors$datasetOption6Label),
                 "SARSHongKong2003")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption6Input))

    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption7Label))
    expect_equal(getText(remDr, pages$state2.2$selectors$datasetOption7Label),
                 "SmallpoxKosovo1972")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$datasetOption7Input))
  })

  test_that("width inputs are displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$incidenceWidthLabel))
    expect_equal(getText(remDr, pages$state2.2$selectors$incidenceWidthLabel),
                 "Choose the width of the sliding time window for R estimation")
  })

  test_that("mean prior input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$meanPriorLabel))
    expect_equal(getText(remDr, pages$state2.2$selectors$meanPriorLabel),
                 "Choose the prior for the mean")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$meanPriorInput))
  })

  test_that("std prior input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$stdPriorLabel))
    expect_equal(getText(remDr, pages$state2.2$selectors$stdPriorLabel),
                 "Choose the prior for the standard deviation")
    expect_true(isDisplayed(remDr, pages$state2.2$selectors$stdPriorInput))
  })

  test_that("relevant control buttons are displayed", {
    expect_false(isDisplayed(remDr, pages$common$selectors$stopButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$prevButton))
    expect_true(isEnabled(remDr, pages$common$selectors$prevButton))
    expect_true(isDisplayed(remDr, pages$common$selectors$nextButton))
    expect_true(isEnabled(remDr, pages$common$selectors$nextButton))
    expect_false(isDisplayed(remDr, pages$common$selectors$goButton))
  })

  test_that("no errors are displaying", {
    expect_true(isDisplayed(remDr, pages$common$selectors$errorMessage))
    expect_equal(getText(remDr, pages$common$selectors$errorMessage), "")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
