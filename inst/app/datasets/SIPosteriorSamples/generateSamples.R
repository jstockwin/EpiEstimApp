# When adding preloaded SI data to the app, we'll always use si_from_samples, since we don't want the user
# to have to wait for MCMC to run for pre-loaded data when we can just save the output. 

# The following code reads raw SI exposure data, and saves the posterior samples that can be used by si_from_sample.

library(devtools)
install_github("nickreich/coarseDataTools", ref = "hackout3")
library(coarseDataTools)
library(MCMCpack)
install_github('annecori/EpiEstim', ref = "hackout3")
library(EpiEstim)

dists <- c("G", "W", "L", "off1G", "off1W", "off1L")
for (file in list.files("datasets/SerialIntervalData")) {
  SIDataPath <- paste('datasets/SerialIntervalData/', file, sep="")
  writePath <- paste('datasets/SIPosteriorSamples/', tools::file_path_sans_ext(file), '_SISamples_', sep="") # Will have `dist.csv` added to the end.

  SIData <- read.csv(SIDataPath)

  SIData <- EpiEstim:::process_si_data(SIData)

  for (dist in dists) {
    init.pars <- init_mcmc_params(SIData, dist=dist)

    fit <- dic.fit.mcmc(SIData, dist=dist, init.pars = init.pars, seed=1)

    CDT <- coarse2estim(fit, dist=dist)

    si_sample <- CDT$si_sample

    write.table(si_sample, file=paste(writePath, dist, ".csv", sep=""), row.names=FALSE, col.names=FALSE, sep=",")
  }
}

