# This utils file consists of necessary functions for data processing and file management. 
# They are in this file so as to not confuse the logic in server.R unnecessarily.

# If any additional datasets are added, the only changes should be additional cases to the if statements
# in the following functions (and an update to ui.R) - the logic in server.R should not have to change.


# The following function takes the string from input$data and uses it to pull in the serialIntervalData
# It is this function which should be edited if any of the serial inteval file names change.
getSerialIntervalData <- function (data) {
  filename <- paste('datasets/SerialIntervalData/', data, '.csv', sep="")
  serialIntervalData <- read.table(filename, header = F, sep=',')
  return(serialIntervalData)
}


# The following function takes the string from input$data and uses it to pull in the incidenceData
# It is this function which should be edited if any of the cases per day file names change.
getIncidenceData <- function (data) {
  filename <- paste('datasets/IncidenceData/', data, '.csv', sep="")
  incidenceData <- read.table(filename, header = FALSE, sep=',')
  return(incidenceData)
}

getSIDistribution <- function(data) {
  filename <- paste('datasets/SerialIntervalDistributions/', data, '.csv', sep="")
  SIDist <- as.numeric(read.table(filename, header=FALSE, sep=','))
  return(SIDist)
}

# The following function takes the string from input$data, and the distirbution from input$SIDist and returns
# the saved fit. It should be updated if any of the file names for the exported fits change.
getSISamples <- function (data, SIDist) {
  filename <- paste('datasets/SIPosteriorSamples/', data, '_SISamples_', SIDist, '.csv', sep='')
  samples <- read.table(filename, header=FALSE, sep=',')
  samples <- as.matrix(samples)
  return(samples)
}

getMCMCProgress <- function(filename) {
  currentIteration <- 0
  tryCatch({
    con <- file(filename, "r")
    line = readLines(con) # Unfortunately we have to read the whole file as there are no line breaks printed...
    close(con)
    progress <- unlist(regmatches(line, gregexpr('iteration ?[0-9]+', line)))
    
    currentIteration <- as.numeric(gsub("iteration ", "", progress[length(progress)]))
    if (length(currentIteration) == 0) {
      currentIteration <- 0 # Fix weird bug where currentIteration = numeric(0) and breaks things.
    }
  },
  error = function(e) {
    # If no file is present, the above will error. This means no progress has been made,
    # so keep currentInteration at 0 (as initialised)
  },
  warning = function(e) {
    # Ignore warnings about file not found
  })
  
  return(currentIteration)
}

