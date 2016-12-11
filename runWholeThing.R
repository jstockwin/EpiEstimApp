library(devtools)
install_github("nickreich/coarseDataTools", ref = "hackout3")
library(coarseDataTools)
library(MCMCpack)
library(EpiEstim)

source("~/Documents/HackoutCode/dic.fit.R")
source("~/Documents/HackoutCode/dic.fit.mcmc.R")
source("~/Documents/HackoutCode/EstimateR.R")
source("~/Documents/HackoutCode/coarse2estim.R")
source("~/Documents/HackoutCode/EstimateRnew.R")
source("~/Documents/HackoutCode/DiscrSI.R")
source("~/Documents/HackoutCode/OverallInfectivity.R")
source("~/Documents/HackoutCode/stochasticSEIRModel3.R")

simulatedData <- simulateTransmissionModel(2)  # contains simulatedData$casesPerDay and simulatedData$dataMatrix
serialIntervalData <- na.omit(simulatedData$dataMatrix)
casesPerDayData <- simulatedData$casesPerDay

names <- c("EL", "ER", "SL", "SR", "type")
colnames(serialIntervalData) <- names
serialIntervalData[,1] = serialIntervalData[,2]
serialIntervalData[,2] = serialIntervalData[,3]
serialIntervalData[,3] = serialIntervalData[,4]
serialIntervalData[,4] = serialIntervalData[,5]
serialIntervalData[,5] = 0

serialIntervalData <- as.data.frame(serialIntervalData)

####  FEED INTO COARSE DATA TOOLS

# Only use 80 host pairs' interval data to estimate the serial interval
fit <- dic.fit.mcmc(dat = serialIntervalData[1:80,], dist="G")



####  FEED INTO EPIESTIM

EstimateR(casesPerDayData[1:100,2], T.Start=5:29, T.End=16:40, n2 = dim(fit@samples)[2], CDT = fit, plot=TRUE)

#  We are getting an error here from the EstimateR code:  "SI.Distr does not sum to 1".  We haven't yet figured out the source of this error. 






#Flu2009$Incidence, T.Start=2:26, T.End=8:32, method = c("NonParametricUncertainSI"),
#SI.Dist.Matrix = c2e$prob_matrix, n2=small_n



# EpiCDT <- function(..., CDT = NULL) {
#   if (!is.null(CDT)) {
#     c2e <- coarse2estim(CDT)
#     EstimateR(..., method = c("NonParametricUncertainSI"),
#                       SI.Dist.Matrix = c2e$prob_matrix)
#   } else {
#     EstimateR(...)
#   }
# }





