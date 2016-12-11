#########################################################
# Discretized serial interval (assuming a shifted gamma #
# distribution (with shift 1)                           #
#########################################################

DiscrSI<-function(k,mu,sigma)
{
  if(sigma<0)
  {
    stop("sigma must be >=0.")
  }
  a=((mu-1)/sigma)^2
  b=sigma^2/(mu-1)
  CDFGamma<-function(k,a,b)
  {
    return(pgamma(k,shape=a,scale=b))
  }
  res<-k*CDFGamma(k,a,b)+(k-2)*CDFGamma(k-2,a,b)-2*(k-1)*CDFGamma(k-1,a,b)
  res<-res+a*b*(2*CDFGamma(k-1,a+1,b)-CDFGamma(k-2,a+1,b)-CDFGamma(k,a+1,b))
  res<-max(0,res)
  return(res)
}

