library(testthat)
library(EpiEstimApp)
library(subprocess)
library(devtools)

# Helper functions:
is_windows <- function () (tolower(.Platform$OS.type) == "windows")

R_binary <- function () {
  R_exe <- ifelse (is_windows(), "R.exe", "R")
  return(file.path(R.home("bin"), R_exe))
}

# Start the server
cat("Starting app\n")
handle <- spawn_process(R_binary(), c('--no-save'))
process_write(handle, "devtools::install_github('nickreich/coarseDataTools', ref='hackout3')\n")
process_write(handle, "devtools::install_github('annecori/EpiEstim', ref='hackout3')\n")
process_write(handle, "EpiEstimApp::runEpiEstimApp()\n")
cat("Running tests\n")
tryCatch({
    test_check("EpiEstimApp")
    cat("Tests finished.\n")
  },
  error = function(e) {
    cat("An error occured.\n")
    cat("\n\n\nPrinting output from app's STDOUT:\n")
    cat(process_read(handle, PIPE_STDOUT, timeout=1000), sep="\n")
    cat("\n\n\nPrinting output from app's STDERR:\n")
    cat(process_read(handle, PIPE_STDERR), sep="\n")
    cat("Throwing error\n")
    stop(e)
  })
cat("Closing App\n")
process_kill(handle)
cat("Done running testthat\n")
