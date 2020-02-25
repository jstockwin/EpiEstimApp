#!/usr/bin/env Rscript
library(testthat)
library(EpiEstimApp)
library(callr)
library(devtools)
library(httr)

# Helper functions:
is_windows <- function () (tolower(.Platform$OS.type) == "windows")

R_binary <- function () {
  R_exe <- ifelse (is_windows(), "R.exe", "R")
  return(file.path(R.home("bin"), R_exe))
}

# Start the server
cat("Starting app\n")
app_process = r_bg(function() {EpiEstimApp::runEpiEstimApp(port=3000)},
                   stdout = "/tmp/out", stderr = "/tmp/err")
cat("Waiting for app to start...\n")
timeout <- 600
t <- 0
while (t < timeout) {
  tryCatch({
      res <- GET("http://localhost:3000")
      if (res$status == 200) {
          break()
      }
  }, error = function(e) {
     # Ignore "could not connect to server" etc
  })
  t <- t + 1
  Sys.sleep(1)
}
cat("App started. Running tests\n")
tryCatch({
    test_check("EpiEstimApp")
    cat("Tests finished.\n")
  },
  error = function(e) {
    cat("An error occured.\n")
    cat("\n\n\nPrinting output from app's STDOUT:\n")
    readLines("/tmp/out")
    cat("\n\n\nPrinting output from app's STDERR:\n")
    readLines("/tmp/err")
    cat("Throwing error\n")
    stop(e)
  })
cat("Closing App\n")
app_process$kill()
cat("Done running testthat\n")
