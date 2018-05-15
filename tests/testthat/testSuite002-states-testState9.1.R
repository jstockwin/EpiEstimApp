context("Test Suite 2 (States) --> State 9.1")

library(RSelenium)
library(testthat)
source("functions.R", local=TRUE)

drivers <- getRemDrivers("Test Suite 2 (States) --> State 9.1")
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

  test_that("can navigate to state 9.1", {
   navigateToState(remDr, "9.1")
  })

  test_that("SI distribution selection input is displaying correctly", {
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$distribution_label),
                 "Choose the serial interval distribution")
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_1_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$distribution_option_1_label),
                 "Gamma")
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_1_input))

    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_2_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$distribution_option_2_label),
                 "Offset Gamma")
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_2_input))

    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_3_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$distribution_option_3_label),
                 "Weibull")
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_3_input))

    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_4_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$distribution_option_4_label),
                 "Offset Weibull")
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_4_input))

    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_5_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$distribution_option_5_label),
                 "Log-Normal")
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_5_input))

    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_6_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$distribution_option_6_label),
                 "Offset Log-Normal")
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$distribution_option_6_input))
  })

  test_that("n1 input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$n1_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$n1_label),
                 'Choose the number of serial interval distributions to be estimated using MCMC (n1)')
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$n1_input))
  })

  test_that("burnin input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$burnin_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$burnin_label),
                 "Choose the number of iterations used as MCMC burn in (burnin)")
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$burnin_input))
  })

  test_that("thin input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$thin_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$thin_label),
                 paste("Choose the MCMC thinning parameter (thin). Each of the n1",
                       "estimated serial interval distributions will be taken after",
                       "every 'thin' iterations of the MCMC chain."))
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$thin_input))
  })

  test_that("n2 input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$n2_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$n2_label),
                 paste("Choose the number of serial interval values to be sampled",
                       "from each estimated serial interval distribution in order to estimate R (n2)"))
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$n2_input))
  })

  test_that("seed input is displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$seed_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$seed_label),
                 paste("Set a seed for the MCMC. If no seed is chosen, a random",
                       "value will be set."))
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$seed_input))
  })

  test_that("init_pars inputs are displayed correctly", {
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$param1_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$param1_label),
                 "Choose the value of param1")
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$param1_input))
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$param2_label))
    expect_equal(getText(remDr, pages$state9.1$selectors$param2_label),
                 "Choose the value of param2")
    expect_true(isDisplayed(remDr, pages$state9.1$selectors$param2_input))
  })

  test_that("relevant control buttons are displayed", {
    expect_false(isDisplayed(remDr, pages$common$selectors$stop_button))
    expect_true(isDisplayed(remDr, pages$common$selectors$prev_button))
    expect_true(isEnabled(remDr, pages$common$selectors$prev_button))
    expect_false(isDisplayed(remDr, pages$common$selectors$next_button))
    expect_true(isDisplayed(remDr, pages$common$selectors$go_button))
    expect_true(isEnabled(remDr, pages$common$selectors$go_button))
  })

  test_that("no errors are displaying", {
    expect_true(isDisplayed(remDr, pages$common$selectors$error_message))
    expect_equal(getText(remDr, pages$common$selectors$error_message), "")
  })
},
error = function(e) {
  closeRemDrivers(remDr, rD)
  stop(e)
})

closeRemDrivers(remDr, rD)
