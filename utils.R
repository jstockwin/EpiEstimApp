# This utils file consists of necessary functions for data processing and file management. 
# They are in this file so as to not confuse the logic in server.R unnecessarily.

# If any additional datasets are added, the only changes should be additional cases to the if statements
# in the following functions (and an update to ui.R) - the logic in server.R should not have to change.


# The following function takes the string from input$data and uses it to pull in the serialIntervalData
# It is this function which should be edited if any of the serial inteval file names change.
getSerialIntervalData <- function (data) {
  if (data == 'PennsylvaniaH1N12009') {
    serialIntervalData <- read.table('datasets/PennsylvaniaH1N12009SerialIntervalData.csv',
                                  header = F, sep=',')
    return(processSerialIntervalData(serialIntervalData))
  } else if (data == 'RotavirusGermany') {
    serialIntervalData <- read.table('datasets/RotavirusEcuadorSIData3.csv',
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
    incidenceData <- read.table('datasets/PennsylvaniaH1N12009FluData.csv',
                                  header = F, sep=',')
    return(processIncidenceData(incidenceData))
  } else if (data == 'RotavirusGermany') {
    incidenceData <- read.table('datasets/GermanyRotavirus1516.csv',
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
    load('datasets/PennsylvaniaH1N12009_si_samples.RData')
    samples <- get(paste('pennsylvaniaH1N12009_samples', SIDist, sep='_'))
  } else if (data == 'RotavirusGermany') {
    load('datasets/Rotavirus_si_samples.RData')
    samples <- get(paste('rotavirus_samples', SIDist, sep='_'))
  } else {
    return(NULL)
  }
}


processSerialIntervalData <- function (serialIntervalData) {
  serialIntervalData <- as.data.frame(serialIntervalData)
  num_cols = dim(serialIntervalData)[2]
  if (num_cols < 4 || num_cols > 5) {
    stop("serialIntervalData should have 4 or 5 columns")
  } else if (num_cols == 4) {
    # Add the type column manually
    serialIntervalData[5] <- 0
  }
  # serialIntervalData will now have 5 columns.
  names <- c("EL", "ER", "SL", "SR", "type")
  colnames(serialIntervalData) <- names
  serialIntervalData <- as.data.frame(serialIntervalData)
}

processIncidenceData <- function (incidenceData, importedData=NULL) {
  incidenceData <- as.data.frame(incidenceData)
  cases_dims <- dim(incidenceData)
  if ((cases_dims[1] == 1 && cases_dims[2] > 1)) {
    # The data is transposed (we want a column vector).
    incidenceData <- t(incidenceData)
    # Update cases_dims for next bit
    cases_dims <- dim(incidenceData)
  }
  
  if (cases_dims[2] > 1) {
    # Bad input
    stop("incidenceData should only have one column")
  }
  incidenceData <- as.data.frame(incidenceData)
  colnames(incidenceData) = "local"
  if (!is.null(importedData)) {
    importedData <- as.data.frame(importedData)
    cases_dims <- dim(importedData)
    if ((cases_dims[1] == 1 && cases_dims[2] > 1)) {
      # The data is transposed (we want a column vector).
      importedData <- t(importedData)
      # Update cases_dims for next bit
      cases_dims <- dim(importedData)
    }
    
    if (cases_dims[2] > 1) {
      # Bad input
      stop("importedData should only have one column")
    }
    
    incidenceData$imported = importedData[,1]
    
    # Currently the "local" column will be the total number of cases because of the way the app
    # is asking for inputs. Correct for this. 
    incidenceData$local = incidenceData$local - incidenceData$imported
  }
  return(incidenceData)
}