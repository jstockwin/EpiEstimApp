#######################################################################################################################
# coarse2estim integates CoarseDataTools with EpiEstim using the amended version of EstimateR called EsimateRAmended2 #
#######################################################################################################################


####  To check...  Should offset gamma distribution start at 1 (i.e. make the first entry zero for that distribution)? YES.  Does estimateR take
# max interval and prob matrix? NO - assumes [0:1:] as the serial intervals corresponding to prob matrix. If so, shouldn't prob matrix be normalised? YES
# At the bottom of this code, an extra row of zeros is added to probMatrix... Why??  This is to ensure that the serial interval can't be 0 days, and so shifts the distribution over by 1.

coarse2estim <- function(object, n_samples=1000){

  samplesInput <- object@samples
  distInput <- object@dist
  
  dist <- distInput
  samples0 <- as.matrix(samplesInput)
  index <- sample(1:nrow(samples0), size= n_samples)
  samples <- samples0[index, ]
  
  ##  Probability matrix that will be used in EpiEstim based on which distribution is specified by the user
  if (dist == "G"){
    
    # For each input parameter set, find the 99th percentile, and take the maximum of these as the maximum
    # serial interval that we need to consider
    maxValue <- ceiling(qgamma(0.999, shape = samples[1,1], scale = samples[1,2]))
    for (i in c(1:n_samples)) {
      if (maxValue < ceiling(qgamma(0.999, shape = samples[i,1], scale = samples[i,2]))) {
        maxValue <- ceiling(qgamma(0.999, shape = samples[i,1], scale = samples[i,2]))
      }
    }
    
    max_interval <- c(10^(-4), 1:maxValue)
    prob_matrix <- apply(samples, 1, function(x) dgamma(max_interval, shape=x[1], scale=x[2]))
    
  } else if (dist == "off1G"){
    
    maxValue <- ceiling(qgamma(0.999, shape = samples[1,1], scale = samples[1,2])) + 1
    for (i in c(1:n_samples)) {
      if (maxValue < (ceiling(qgamma(0.999, shape = samples[i,1], scale = samples[i,2])) + 1)) {
        maxValue <- (ceiling(qgamma(0.999, shape = samples[i,1], scale = samples[i,2])) + 1)
      }
    }
    
    # offset gamma distribution with shifted min and max value of max serial interval
    max_interval <- c(10^(-4), 1:maxValue)
    prob_matrix <- apply(samples, 1, function(x) dgamma(max_interval - 1, shape=x[1], scale=x[2]))
    
    prob_matrix[1,] <- 0  # since 0 day serial interval is impossible with offset gamma distribution
    
  } else if (dist == "E"){
    
    maxValue <- ceiling(qgamma(0.999, shape = samples[1,1], scale = samples[1,2]))
    for (i in c(1:n_samples)) {
      if (maxValue < ceiling(qgamma(0.999, shape = samples[i,1], scale = samples[i,2]))) {
        maxValue <- ceiling(qgamma(0.999, shape = samples[i,1], scale = samples[i,2]))
      }
    }
    
    # Erlang distribution
    max_interval <- c(10^(-4), 1:maxValue)
    prob_matrix <- apply(samples, 1, function(x) dgamma(max_interval, shape=x[1], scale=x[2]))
    
  }
  else if (dist == "W"){
    
    maxValue <- ceiling(qweibull(0.999, shape = samples[1,1], scale = samples[1,2]))
    for (i in c(1:n_samples)) {
      if (maxValue < ceiling(qweibull(0.999, shape = samples[i,1], scale = samples[i,2]))) {
        maxValue <- ceiling(qweibull(0.999, shape = samples[i,1], scale = samples[i,2]))
      }
    }

    max_interval <- c(10^(-4), 1:maxValue)
    prob_matrix <- apply(samples, 1, function(x) dweibull(max_interval, shape=x[1], scale=x[2]))
    
  } else if (dist == "L"){
    
    maxValue <- ceiling(qlnorm(0.999, meanlog = samples[1,1], sdlog = samples[1,2]))
    for (i in c(1:n_samples)) {
      if (maxValue < ceiling(qlnorm(0.999, meanlog = samples[i,1], sdlog = samples[i,2]))) {
        maxValue <- ceiling(qlnorm(0.999, meanlog = samples[i,1], sdlog = samples[i,2]))
      }
    }
    
    #max_interval <- c(10^(-4), 1:ceiling(qgamma(0.999, shape=object@ests[1,1], scale=object@ests[2,1])))
    max_interval <- c(10^(-4), 1:maxValue)
    prob_matrix <- apply(samples, 1, function(x) dlnorm(max_interval, meanlog=x[1], sdlog=x[2]))
  } else {
    stop(sprintf("Distribtion (%s) not supported",dist))
  }
  prob_matrix <- apply(prob_matrix, 2, function(x) x/sum(x))
  
  prob_matrix <- rbind(rep(0, ncol(prob_matrix)), prob_matrix)  # Do I really want this extra row of 0's?
  out <- list(prob_matrix = prob_matrix, dist = dist)
  
  return(out)
}

