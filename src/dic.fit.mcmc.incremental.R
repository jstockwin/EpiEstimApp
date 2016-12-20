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
                                      ...){
  
  if (is.null(current.samples)) {
    my.n.samples = min(increment.size, n.samples + burnin)
    samples = dic.fit.mcmc(dat,
                           prior.par1 = prior.par1,
                           prior.par2 = prior.par2,
                           init.pars = init.pars,
                           ptiles = ptiles,
                           verbose=verbose,#how often to print update
                           burnin = 0,
                           n.samples = my.n.samples,
                           dist = dist,
                           ...)@samples
    return(samples)
  } else {
    current.sample.length = dim(current.samples)[1]
    print(current.sample.length)
    my.n.samples = min(increment.size, n.samples + burnin - current.sample.length)
    init.par1 = current.samples[current.sample.length, 1]
    init.par2 = current.samples[current.sample.length, 2]
    my.init.pars = c(init.par1, init.par2)
    samples = dic.fit.mcmc(dat,
                           prior.par1 = prior.par1,
                           prior.par2 = prior.par2,
                           init.pars = init.pars,
                           ptiles = ptiles,
                           verbose=verbose,#how often to print update
                           burnin = 0,
                           n.samples = my.n.samples,
                           dist = dist,
                           ...)@samples
    return(rbind(current.samples, samples))
  }
}