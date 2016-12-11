library(devtools)
install_github("nickreich/coarseDataTools", ref = "hackout3")
library(coarseDataTools)
library(MCMCpack)
library(EpiEstim)

source("dic.fit.R")
source("dic.fit.mcmc.R")
source("EstimateR.R")
source("coarse2estim.R")
source("EstimateRnew.R")
source("DiscrSI.R")
source("OverallInfectivity.R")
source("stochasticSEIRModel3.R")

transmissibility_estimation <- function (serialIntervalData, casesPerDayData, W) {
  
  ##############################################
  ## Input data validation and pre-processing ##
  ##############################################
  
  ## Pre-process the serialIntervalData ##
  
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
  
  ## Pre-process the casesPerDayData
  
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
  
  #######################
  ## Run the functions ##
  #######################
  
  # Only use 80 host pairs' interval data to estimate the serial interval
  fit <- dic.fit.mcmc(dat = serialIntervalData, dist="G")

  ####  FEED INTO EPIESTIM
  
  a <- EstimateR(casesPerDayData[,2], T.Start=1:(cases_dims[1] - W), T.End=(1+W):cases_dims[1], n2 = dim(fit@samples)[2], CDT = fit, plot=TRUE)

}

