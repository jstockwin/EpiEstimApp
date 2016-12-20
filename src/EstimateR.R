#########################################################################################################################
# EstimateR is a wraper which replace the old EstimateR with EstimateR_new and accepts an object of class "cd.fit.mcmc" #
#########################################################################################################################

EstimateR <- function(..., CDT = NULL, dist = NULL) {
  if (!is.null(CDT) && !is.null(dist)) {
    c2e <- coarse2estim(CDT, dist)
    EstimateR_new(..., method = c("NonParametricUncertainSI"),
                  SI.Dist.Matrix = c2e$prob_matrix)
  } else {
    EstimateR_new(...)
  }
}

