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
    return(processSerialIntervalData(serialIntervalData))
  } else if (data == 'RotavirusGermany') {
    serialIntervalData <- read.table('datasets/SerialIntervalData/RotavirusEcuadorSIData.csv',
                                  header = F, sep=',')
    return(processSerialIntervalData(serialIntervalData))
  } else {
    return(NULL)
  }
}


# The following function takes the string from input$data and uses it to pull in the incidenceData
# It is this function which should be edited if any of the cases per day file names change.
getIncidenceData <- function (data, alldatasets) {
  if (data %in% names(alldatasets)) {
    dat <- alldatasets[[data]]$Incidence
    return(processIncidenceData(dat))
  } else  if (data == 'PennsylvaniaH1N12009') {
    incidenceData <- read.table('datasets/IncidenceData/PennsylvaniaH1N12009FluData.csv',
                                  header = F, sep=',')
    return(processIncidenceData(incidenceData))
  } else if (data == 'RotavirusGermany') {
    incidenceData <- read.table('datasets/IncidenceData/GermanyRotavirus1516.csv',
                                  header = F, sep=',')
    return(processIncidenceData(incidenceData))
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
  } else if (data == 'RotavirusGermany') {
    if (SIDist %in% c("off1G", "off1W", "off1L")) {
      stop('The Rotavirus dataset has serial intervals which are definitely less than 1, so an offset distribution is not appropriate.')
    }
    samples <- read.table((paste('datasets/SIPosteriorSamples/rotavirus_SISamples_', SIDist, '.csv', sep='')),
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

prepSauceConnect <- function(name) {
  if (!exists("sauceUsername")) {
    if (Sys.getenv("SAUCE_USERNAME") != "") {
      user <- Sys.getenv("SAUCE_USERNAME") # Your Sauce Labs username
    } else {
      stop("You must provide a username for saucelabs. Set the sauceUsername variable in R")
    }
  } else {
    user <- sauceUsername
  }
  
  if (!exists("sauceAccessKey")) {
    if (Sys.getenv("SAUCE_USERNAME") != "") {
      pass <- Sys.getenv("SAUCE_USERNAME") # Your Sauce Labs username
    } else {
      stop("You must provide an access key for saucelabs. Set the sauceAccessKey variable in R")
    }
  } else {
    pass <- sauceAccessKey
  }
  
  port <- 4445 
  ip <- paste0(user, ':', pass, "@localhost")
  extraCapabilities <- list(name = name, username = user, accessKey = pass
                            , startConnect = FALSE, tunnelIdentifier = Sys.getenv("TRAVIS_JOB_NUMBER"))
  remDr <- remoteDriver$new(remoteServerAddr = ip, port = port, extraCapabilities = extraCapabilities)
  return(remDr)
}