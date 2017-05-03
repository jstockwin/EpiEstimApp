# This utils file consists of necessary functions for data processing and file management. 
# They are in this file so as to not confuse the logic in server.R unnecessarily.

# If any additional datasets are added, the only changes should be additional cases to the if statements
# in the following functions (and an update to ui.R) - the logic in server.R should not have to change.


# The following function takes the string from input$data and uses it to pull in the serialIntervalData
# It is this function which should be edited if any of the serial inteval file names change.
getSerialIntervalData <- function (data) {
  if (data == 'PennsylvaniaH1N12009') {
    serialIntervalData <- read.table('dataset/SerialIntervalDatas/PennsylvaniaH1N12009SerialIntervalData.csv',
                                  header = F, sep=',')
    return(serialIntervalData)
  } else {
    return(NULL)
  }
}


# The following function takes the string from input$data and uses it to pull in the incidenceData
# It is this function which should be edited if any of the cases per day file names change.
getIncidenceData <- function (data, alldatasets) {
  if (data %in% names(alldatasets)) {
    dat <- alldatasets[[data]]$Incidence
    return(dat)
  } else  if (data == 'PennsylvaniaH1N12009') {
    incidenceData <- read.table('datasets/IncidenceData/PennsylvaniaH1N12009FluData.csv',
                                  header = F, sep=',')
    return(incidenceData)
  } else {
    return(NULL)
  }
}

# The following function takes the string from input$data, and the distirbution from input$SIDist and returns
# the saved fit. It should be updated if any of the file names for the exported fits change.
getSISamples <- function (data, SIDist) {
  if (data == 'PennsylvaniaH1N12009') {
    samples <- read.table((paste('datasets/SIPosteriorSamples/pennsylvaniaH1N12009_SISamples_', SIDist, '.csv', sep='')),
                          header=F, sep=',')
    samples <- as.matrix(samples)
  } else {
    return(NULL)
  }
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

