# When adding preloaded SI data to the app, we'll always use SIFromSamples, since we don't want the user
# to have to wait for MCMC to run for pre-loaded data when we can just save the output. 

# The following code reads raw SI exposure data, and saves the posterior samples that can be used by SIFromSample.

library(devtools)
install_github("robin-thompson/coarseDataTools", ref = "hackout3")
library(coarseDataTools)
library(MCMCpack)
install_github('annecori/EpiEstim', ref = "hackout3")
library(EpiEstim)

SIDataPath <- 'datasets/SerialIntervalData/PennsylvaniaH1N12009SerialIntervalData.csv'
dist <- 'off1L'
writePath <- 'datasets/SIPosteriorSamples/pennsylvaniaH1N12009_SISamples_' # Will have `dist.csv` added to the end.

SIData <- read.csv(SIDataPath)

SIData <- processSerialIntervalData(SIData)

init.pars <- init_MCMC_params(SIData, dist=dist)

fit <- dic.fit.mcmc(SIData, dist=dist, init.pars = init.pars)

CDT <- coarse2estim(fit, dist=dist)

SI.Sample <- CDT$SI.Sample

write.table(SI.Sample, file=paste(writePath, dist, ".csv", sep=""), row.names=FALSE, col.names=FALSE, sep=",")
