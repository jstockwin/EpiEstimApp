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

serialIntervalData2 <-  read.csv("/Users/rnt22/Documents/HackoutCode/datasets/2009H1N1SIData.csv", header = FALSE)

serialIntervalData <- matrix(NA, nrow = 16, ncol = 5)

for (row in 1:16) {
  
  serialIntervalData[row,1] <- 0
  serialIntervalData[row,2] <- 1
  serialIntervalData[row,3] <- serialIntervalData2[row, 1] + 1
  serialIntervalData[row,4] <- serialIntervalData2[row, 2] 
  serialIntervalData[row,5] <- 0
  
}

names <- c("EL", "ER", "SL", "SR", "type")
colnames(serialIntervalData) <- names

serialIntervalData <- as.data.frame(serialIntervalData)

####  FEED INTO COARSE DATA TOOLS


#for (i in c(1:19)) {
#  if (serialIntervalData[i,1] == serialIntervalData[i,3]){
#    serialIntervalData[i,3] = serialIntervalData[i,3] + 1
#    serialIntervalData[i,4] = serialIntervalData[i,4] + 1
#  }
#}

# Only use 80 host pairs' interval data to estimate the serial interval
fit <- dic.fit.mcmc(dat = serialIntervalData[1:16,], dist="G")


casesPerDayData <- read.csv("/Users/rnt22/Documents/HackoutCode/datasets/H1N12009FluData.csv", header = FALSE)
casesPerDayData <- t(casesPerDayData)
casesPerDayData <- cbind(1:31, casesPerDayData)
casesPerDayData <- as.data.frame(casesPerDayData)
names <- c("Time", "Cases")
colnames(casesPerDayData) <- names




####  FEED INTO EPIESTIM

a <- EstimateR(casesPerDayData[1:31,2], T.Start=1:26, T.End=6:31, n2 = dim(fit@samples)[2], CDT = fit, plot=TRUE)








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





