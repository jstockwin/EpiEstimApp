#########################################################
# Calculates Lambda_t = Sum_1^t I_{t-s} * w_s           #
# with I incidence and w discrete SI distribution       #
#########################################################

OverallInfectivity <-function (I,SI.Distr)
{
  if(is.vector(I)==FALSE)
  {
    stop("Incidence must be a vector.")
  }
  T<-length(I)
  for(i in 1:T)
  {
    if(I[i]<0)
    {
      stop("Incidence must be a positive vector.")
    }
  }
  if(is.vector(SI.Distr)==FALSE)
  {
    stop("SI.Distr must be a vector.")
  }
  if(SI.Distr[1]!=0)
  {
    stop("SI.Distr[1] needs to be 0.")
  }
  if(length(SI.Distr)>1)
  {
    for(i in 2:length(SI.Distr))
    {
      if(SI.Distr[i]<0)
      {
        stop("SI.Distr must be a positive vector.")
      }
    }
  }
  if(abs(sum(SI.Distr)-1)>0.01)
  {
    stop("SI.Distr must sum to 1.")
  }
  lambda <- vector()
  lambda[1]<-NA
  for (t in 2:length(I))
  {
    lambda[t] <- sum(SI.Distr[1:t]*I[t:1],na.rm=TRUE)
  }
  return(lambda)
}

