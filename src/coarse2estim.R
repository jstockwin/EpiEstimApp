#######################################################################################################################
# coarse2estim integates CoarseDataTools with EpiEstim using the amended version of EstimateR called EsimateRAmended2 #
#######################################################################################################################


####  To check...  Should offset gamma distribution start at 1 (i.e. make the first entry zero for that distribution)? YES.  Does estimateR take
# max interval and prob matrix? NO - assumes [0:1:] as the serial intervals corresponding to prob matrix. If so, shouldn't prob matrix be normalised? YES
# At the bottom of this code, an extra row of zeros is added to probMatrix... Why??  This is to ensure that the serial interval can't be 0 days, and so shifts the distribution over by 1.

coarse2estim <- function(samples, dist, n_samples=1000){

  samples0 <- as.matrix(samples)
  index <- sample(1:nrow(samples0), size= n_samples)
  samples <- samples0[index, ]
  
  ##  Probability matrix that will be used in EpiEstim based on which distribution is specified by the user
  if (dist == "G" | dist == "E"){
  	# For each input parameter set, find the 99th percentile, and take the maximum of these as the maximum
    # serial interval that we need to consider
    maxValue <- max( sapply(1:n_samples, function(i) ceiling(qgamma(0.999, shape = samples[i,1], scale = samples[i,2])) ) )
    max_interval <- 1:maxValue
    prob_matrix <- apply(samples, 1, function(x) pgamma(max_interval+0.5, shape=x[1], scale=x[2]) - pgamma(max_interval-0.5, shape=x[1], scale=x[2]))
  } else if (dist == "off1G"){
    # offset gamma distribution with shifted min and max value of max serial interval
    maxValue <- max( sapply(1:n_samples, function(i) ceiling(qgamma(0.999, shape = samples[i,1], scale = samples[i,2])) ) )
    max_interval <- 0:maxValue
    prob_matrix <- apply(samples, 1, function(x) pgamma(max_interval+0.5, shape=x[1], scale=x[2]) - pgamma(max_interval-0.5, shape=x[1], scale=x[2]))
  } else if (dist == "W"){
  	maxValue <- max( sapply(1:n_samples, function(i) ceiling(qweibull(0.999, shape = samples[i,1], scale = samples[i,2])) ) )
    max_interval <- 1:maxValue
    prob_matrix <- apply(samples, 1, function(x) pweibull(max_interval+0.5, shape=x[1], scale=x[2]) - pweibull(max_interval-0.5, shape=x[1], scale=x[2]))
  } else if (dist == "L"){
  	maxValue <- max( sapply(1:n_samples, function(i) ceiling(qlnorm(0.999, meanlog = samples[i,1], sdlog = samples[i,2])) ) )
    max_interval <- 1:maxValue
    prob_matrix <- apply(samples, 1, function(x) plnorm(max_interval+0.5, meanlog=x[1], sdlog=x[2]) - plnorm(max_interval-0.5, meanlog=x[1], sdlog=x[2]))
  } else {
    stop(sprintf("Distribtion (%s) not supported",dist))
  }
  # adding initial 0 for P(SI=0)
  prob_matrix <- rbind(rep(0, n_samples), prob_matrix)
  # renormalising
  prob_matrix <- apply(prob_matrix, 2, function(x) x/sum(x))
  
  out <- list(prob_matrix = prob_matrix, dist = dist)
  
  return(out)
}

