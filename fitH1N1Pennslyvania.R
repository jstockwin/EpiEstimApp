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

serialIntervalData <-  read.csv("datasets/PennsylvaniaH1N12009SerialIntervalData.csv", header = FALSE)

serialIntervalData[5] <- 0

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
fit <- dic.fit.mcmc(dat = serialIntervalData, dist="G")


casesPerDayData <- read.csv("datasets/PennsylvaniaH1N12009FluData.csv", header = FALSE)
casesPerDayData <- t(casesPerDayData)
casesPerDayData <- cbind(seq.int(nrow(casesPerDayData)), casesPerDayData)
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





