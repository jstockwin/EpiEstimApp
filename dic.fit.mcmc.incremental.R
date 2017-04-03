dic.fit.mcmc.incremental <- function (dat,
                                      current.samples=NULL,#current progress
                                      increment.size = 10,
                                      prior.par1 = NULL,
                                      prior.par2 = NULL,
                                      init.pars = c(1,1),
                                      ptiles = c(0.05,0.95,0.99),
                                      verbose=FALSE,#how often to print update
                                      burnin = 3000,
                                      n.samples = 5000,
                                      dist = "L",
                                      seed = NULL,
                                      ...){
  
  if (is.null(current.samples)) {
    my.n.samples = min(increment.size, n.samples + burnin)
    my.init.pars = init.pars
  } else {
    current.sample.length = dim(current.samples)[1]
    my.n.samples = min(increment.size, n.samples + burnin - current.sample.length)
    init.par1 = current.samples[current.sample.length, 1]
    init.par2 = current.samples[current.sample.length, 2]
    my.init.pars = c(init.par1, init.par2)
  }

  
  # MCMCPack sets the seed, which means we get the same sequence of random numbers every time this is run. 
  # This is pretty bad for incremental, as for the default increment.size of 10, this means our "random" numbers
  # will be the same sequence of 10 numbers each time, which will bias the result hugely. 
  # dic.fit.mcmc are thinking about solving this, see: https://github.com/nickreich/coarseDataTools/issues/45
  # For now we'll set the seed using the epoch time
  if (is.null(seed)) {
    t <- as.numeric(Sys.time())
    seed <- 1e8 * (t - floor(t))
  }



  fit = dic.fit.mcmc(dat,
                         prior.par1 = prior.par1,
                         prior.par2 = prior.par2,
                         init.pars = my.init.pars,
                         ptiles = ptiles,
                         verbose=verbose,#how often to print update
                         burnin = 0,
                         n.samples = my.n.samples,
                         dist = dist,
                         seed = seed,
                         ...)
  
  fit@samples = rbind(current.samples, fit@samples)

  
  return(fit)

  
}
