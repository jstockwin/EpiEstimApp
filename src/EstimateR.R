#########################################################################################################################
# EstimateR is a wraper which replace the old EstimateR with EstimateR_new and accepts an object of class "cd.fit.mcmc" #
#########################################################################################################################

EstimateR <- function(..., CDT = NULL, dist = NULL) {
  if (!is.null(CDT)) {
    if (class(CDT)[1]!="cd.fit.mcmc")
      warning("CDT needs to be defined as an object of the S4 class 'cd.fit.mcmc??")
    c2e <- coarse2estim(CDT)
    EstimateR_new(..., method = c("NonParametricUncertainSI"),
                  SI.Dist.Matrix = c2e$prob_matrix)
  } else {
    EstimateR_new(...)
  }
}

