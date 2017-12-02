# This utils file consists of necessary functions for data processing and file
# management.  They are in this file so as to not confuse the logic in server.R
# unnecessarily.

# If any additional datasets are added, the only changes should be additional
# cases to the if statements in the following functions (and an update to ui.R).
# The logic in server.R should not have to change.


# The following function takes the string from input$data and uses it to pull
# in the serial_inteval_data, It is this function which should be edited if any
# of the serial inteval file names change.
get_serial_interval_data <- function (data) {
  filename <- paste("datasets/SerialIntervalData/", data, ".csv", sep = "")
  serial_interval_data <- read.table(filename, header = F, sep = ",")
  return(serial_interval_data)
}


# The following function takes the string from input$data and uses it to pull
# in the incidence_data. It is this function which should be edited if any of
# the cases per day file names change.
get_incidence_data <- function (data) {
  filename <- paste("datasets/IncidenceData/", data, ".csv", sep = "")
  incidence_data <- read.table(filename, header = FALSE, sep = ",")
  return(incidence_data)
}

get_si_distribution <- function(data) {
  filename <- paste("datasets/SerialIntervalDistributions/", data, ".csv",
                    sep = "")
  si_dist <- as.numeric(read.table(filename, header = FALSE, sep = ","))
  return(si_dist)
}

# The following function takes the string from input$data, and the distirbution
# from input$si_dist and returns the saved fit. It should be updated if any of
# the file names for the exported fits change.
get_si_samples <- function (data, si_dist) {
  filename <- paste("datasets/SIPosteriorSamples/", data, "_SISamples_",
                    si_dist, ".csv", sep = "")
  samples <- read.table(filename, header = FALSE, sep = ",")
  samples <- as.matrix(samples)
  return(samples)
}

get_mcmc_progress <- function(filename) {
  current_iteration <- 0
  tryCatch({
    con <- file(filename, "r")
    line <- readLines(con) # Unfortunately we have to read the whole file as
                          # there are no line breaks printed...
    close(con)
    progress <- unlist(regmatches(line, gregexpr("iteration ?[0-9]+", line)))
    
    current_iteration <- as.numeric(gsub("iteration ", "",
                                         progress[length(progress)]))
    if (length(current_iteration) == 0) {
      # Fix weird bug where current_iteration = numeric(0) and breaks things.
      current_iteration <- 0
    }
  },
  error = function(e) {
    # If no file is present, the above will error. This means no progress has
    # been made, so keep currentInteration at 0 (as initialised)
  },
  warning = function(e) {
    # Ignore warnings about file not found
   })

  return(current_iteration)
}
