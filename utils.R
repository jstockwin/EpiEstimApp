processSerialIntervalData <- function (serialIntervalData) {
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

processCasesPerDayData <- function (casesPerDayData) {
  cases_dims <- dim(casesPerDayData)
  if ((cases_dims[1] == 1 && cases_dims[2] > 1) || (cases_dims[1] == 2 && cases_dims[2] > 2)) {
    # The data is transposed.
    casesPerDayData <- t(casesPerDayData)
    # Update cases_dims for next bit
    cases_dims <- dim(casesPerDayData)
  }
  
  if (cases_dims[2] > 2) {
    # Bad input
    stop("casesPerDayData should only have one column, or one column and an index column")
  } else if (cases_dims[2] == 1) {
    # Add a time column first.
    casesPerDayData <- cbind(seq.int(nrow(casesPerDayData)), casesPerDayData)
  }
  colnames(casesPerDayData) <- c("Time", "Cases")
  casesPerDayData <- as.data.frame(casesPerDayData)
}