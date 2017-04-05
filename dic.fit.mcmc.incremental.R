dic.fit.mcmc.incremental <- function (dat,
                                      current.samples=NULL,#current progress
                                      increment.size = 80,
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



  fit = coarseDataTools::dic.fit.mcmc(dat,
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


compute_V <- function(fun, theta.init, 
                      tune = 1, logfun = TRUE, force.samp = FALSE, 
                      optim.method = "BFGS", optim.lower = -Inf, optim.upper = Inf, 
                      optim.control = list(fnscale = -1, trace = 0, REPORT = 10, 
                                           maxit = 500), prior.par1=NULL, prior.par2=NULL, dat, dist, ...)
{
  theta.init.0 <- rep(NA, length(theta.init))
  for (i in 1:length(theta.init)) {
    theta.init.0[i] <- theta.init[i]
  }
  MCMCpack:::check.offset(list(...))
  tune <- MCMCpack:::vector.tune(tune, length(theta.init.0))
  userfun <- function(ttt) fun(ttt, ...)
  my.env <- environment(fun = userfun)
  if (logfun) {
    maxfun <- fun
  }else if (logfun == FALSE) {
    maxfun <- function(ttt, ...) log(fun(ttt, ...))
  }else {
    cat("logfun not a logical value.\n")
    stop("Respecifiy and call MCMCmetrop1R() again. \n", 
         call. = FALSE)
  }
  opt.out <- optim(theta.init.0, maxfun, control = optim.control, 
                   lower = optim.lower, upper = optim.upper, method = optim.method, 
                   hessian = TRUE, prior.par1=prior.par1, prior.par2=prior.par2, dat=dat, dist=dist, ...)
  if (opt.out$convergence != 0) {
    warning("Mode and Hessian were not found with call to optim().\nSampling proceeded anyway. \n")
  }
  CC <- NULL
  try(CC <- chol(-1 * opt.out$hessian), silent = TRUE)
  hess.new <- opt.out$hessian
  hess.flag <- 0
  if (force.samp == TRUE) {
    if (max(diag(opt.out$hessian) == 0)) {
      for (i in 1:nrow(hess.new)) {
        if (hess.new[i, i] == 0) {
          hess.new[i, i] <- -1e-06
        }
      }
    }
    while (is.null(CC)) {
      hess.flag <- 1
      hess.new <- hess.new - diag(diag(0.01 * abs(opt.out$hessian)))
      try(CC <- chol(-1 * hess.new), silent = TRUE)
    }
  }
  else {
    if (is.null(CC)) {
      hess.flag <- 2
    }
  }
  if (hess.flag == 1) {
    warning("Hessian from call to optim() not negative definite.\nSampling proceeded after enforcing negative definiteness. \n")
  }
  if (hess.flag == 2) {
    cat("Hessian from call to optim() not negative definite.\n")
    cat("Sampling (as specified) cannot proceed.\n")
    stop("Check data and fun() and call MCMCmetrop1R() again. \n", 
         call. = FALSE)
  }
  V <- tune %*% solve(-1 * hess.new) %*% tune
  return(V)
}
