context("Test Suite 2 (States) --> State 6.2")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 6.2")
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

  test_that("can navigate to state 6.2", {
    navigateToState(remDr, "6.2")
  })

  test_that("only state 6.2 is displayed", {
    checkDisplayedState(remDr, "6.2")
  })

  test_that("SIEstType input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state6.2$selectors$SIEstType))
    expect_true(isDisplayed(remDr, pages$state6.2$selectors$SIEstTypeLabel))
    expect_equal(getText(remDr, pages$state6.2$selectors$SIEstTypeLabel),
                 "Which of the following serial interval distribution estimates would you like to use?")

    expect_true(isDisplayed(remDr, pages$state6.2$selector$SIEstTypeOption1Label))
    expect_equal(getText(remDr, pages$state6.2$selector$SIEstTypeOption1Label),
                 "Parametric with uncertainty (offset gamma)")

    expect_true(isDisplayed(remDr, pages$state6.2$selector$SIEstTypeOption2Label))
    expect_equal(getText(remDr, pages$state6.2$selector$SIEstTypeOption2Label),
                 "Parametric without uncertainty (offset gamma)")

    expect_true(isDisplayed(remDr, pages$state6.2$selector$SIEstTypeOption3Label))
    expect_equal(getText(remDr, pages$state6.2$selector$SIEstTypeOption3Label),
                 "Upload your own probability distribution")

    expect_true(isDisplayed(remDr, pages$state6.2$selector$SIEstTypeOption4Label))
    expect_equal(getText(remDr, pages$state6.2$selector$SIEstTypeOption4Label),
                 "Use a distribution estimated from a previous outbreak (preloaded data)")
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
