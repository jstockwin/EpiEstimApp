library(coarseDataTools)

library(MCMCpack)
library(EpiEstim)
library(shiny)
library(shinyjs)
library(ggplot2)
library(graphics)
library(grid)
library(gridExtra)
library(plotly)
library(plyr)
library(reshape2)
library(stats)
library(future)

plan(multiprocess)


# Source necessary files
source("utils.R", local=TRUE)
#source("estimater.R", local=TRUE)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)
options(shiny.reactlog=TRUE) 

allStates = c("1.1", "2.1", "2.2", "3.1", "4.1", "5.1", "6.1", "6.2", "7.1", "7.2", "7.3", "7.4",
              "7.5", "7.6", "8.1", "8.2", "8.3", "9.1")

finalStates = c("7.3", "7.4", "7.5", "7.6", "8.1", "8.3", "9.1")

# If the app has crashed we may be left with MCMC progress files, which would
# throw off our counts of how many MCMC processes are running. 
# To be sure this doesn't happen, we will clear the repsective folders
# when this happens.
mcmcProgressFolder <- "mcmc-progress/progress/"
mcmcPidFolder <- "mcmc-progress/pid/"

if (length(list.files(path=mcmcProgressFolder, pattern="*.txt")) > 0) {
  file.remove(paste(mcmcProgressFolder, list.files(path=mcmcProgressFolder, pattern="*.txt"), sep=""))
}
if (length(list.files(path=mcmcPidFolder, pattern="*.txt")) > 0) {
  file.remove(paste(mcmcPidFolder, list.files(path=mcmcPidFolder, pattern="*.txt"), sep=""))
}

shinyServer(function(input, output, session) {
  # DEBUG
  #observe({
  #  cat(values$state, "\n")
  #})
  
  
  # Initialise some reactive values
  t <- as.numeric(Sys.time())
  id <- 1e8 * (t - floor(t))
  id <- gsub("\\.", "-", as.character(id))
  progressFile <- paste(mcmcProgressFolder, id, "-progress.txt", sep="")
  pidFile <- paste(mcmcPidFolder, id, "-pid.txt", sep="")
  values <- reactiveValues(state="1.1", status="Ready", error = NULL)
  enable("nxt") # Enable next button when initial load is done.
  
  asyncData <-
    reactiveValues(epiEstimOutput = NULL, mcmc_samples = NULL, SI.Sample.From.Data = NULL, convergenceCheck = NULL)
  asyncDataBeingLoaded <- list()
  
  # Initialise inputs for EpiEstim's EstimateR
  ## TODO - if we make these reactive, it might mean calling EstimateR twice without changing inputs
  ## doesn't bother to run the second time, which might be nice?
  IncidenceData = NULL
  T.Start = NULL
  T.End = NULL
  method = NULL
  n1 = NULL
  n2 = 100
  Mean.SI = NULL
  Std.SI = NULL
  Std.Mean.SI = NULL
  Min.Mean.SI = NULL
  Max.Mean.SI = NULL
  Std.Std.SI = NULL
  Min.Std.SI = NULL
  Max.Std.SI = NULL
  SI.Distr = NULL
  SI.Data = NULL
  SI.parametricDistr = NULL
  burnin = 3000
  SI.Sample = NULL
  plot = FALSE
  total.samples.needed <- 1 # Will be overridden. Set to 1 so dim(mcmc_samples) < total.samples.needed initially
  mcmc_samples <- NULL
  init.pars <- NULL
  thin <- NULL
  SI.Sample.From.Data <- NULL
  convergenceCheck <- NULL
  requestedSeed <- NULL
  requestedMCMCSeed <- NULL
  Mean.Prior <- 5
  Std.Prior <- 5

  # Clicking previous/next should increment the stateLevel
  observeEvent(input$nxt, {
    # WARNING: You probably want to avoid much logic here. Most of it should be in handleState() which is reactive. 
    # If Next is pressed twice without inputs changing, nothing will happen, but if anything you put here WILL get done.
    if (handleState()) {
      values$state = getNextState(values$state)
      if (values$state == "5.1") {
        hide("incidenceTitle")
        show("SITitle")
      }
      values$status = "Ready"
    }
  })
  observeEvent(input$prev, {
    if (values$state == "5.1") {
      show("incidenceTitle")
      hide("SITitle")
    }
    values$state = getPrevState(values$state)
    values$error = NULL
    session$sendCustomMessage(type="resetErrorBoxes", "")
  })
  
  # Whenever the state changes, toggle which fields are/are not visible.
  observe({
    for (someState in allStates) {
      toggle(someState, condition = someState == values$state)
    }
    toggle("nxt", condition = !(values$state %in% finalStates)) # Hide/show next button as appropriate
    toggle("go", condition = values$state %in% finalStates) # Hide/show go button as appropriate
    toggleState("prev", condition = !(values$state == "1.1")) # Disable/endable prev button as appropriate
  })
  
  # Keep the output text to values$status
  output$output <- renderText({values$status})
  output$progress <- renderText({paste(
      "<p>Step ", substr(values$state,1,1), " of at most 9.",
      " View the <a href='https://github.com/jstockwin/EpiEstimApp/wiki/Interactive-Documentation-State-",
      values$state, "' target='_blank'>interactive documentation</a> for this state.</p>"
      , sep="")})
  output$error <- renderText({values$error})
  
  
  
  # Logic for when "go" is clicked.
  observeEvent(input$go, {
    # Clear the current EpiEstim data (will remove current plot)
    asyncData$epiEstimOutput <- NULL
    run()
    })
  
  run <- function() { #TODO - can we make this async? Might be a bit more tricky...
      # WARNING: You probably want to avoid much logic here. Most of it should be in handleState() which is reactive. 
      # If Next is pressed twice without inputs changing, nothing will happen, but if anything you put here WILL get done.
      tryCatch({
        if (handleState()) {
          # The following sets a seed randomly if no seed was requested.
          # We're using the "requested" seed to ensure that if no seed is
          # requested then a NEW random seed is chosen on EACH run.
          if (is.null(requestedSeed) || is.na(requestedSeed)) {
              t <- as.numeric(Sys.time())
              seed <- 1e8 * (t - floor(t))
          } else {
              seed <- requestedSeed
          }

          if (method=="SIFromData" && is.null(mcmc_samples)) {
            values$status <- "Running MCMC (0%)"
            startAsyncDataLoad("mcmc_samples", future({
              # The following sets a seed randomly if no seed was requested.
              # We're using the "requested" seed to ensure that if no seed is
              # requested then a NEW random seed is chosen on EACH run.
              if (is.null(requestedMCMCSeed) || is.na(requestedMCMCSeed)) {
                  t <- as.numeric(Sys.time())
                  MCMCSeed <- 1e8 * (t - floor(t))
              } else {
                  MCMCSeed <- requestedMCMCSeed
              }
              if (.Platform$OS.type == "unix") {
                write(Sys.getpid(), file=pidFile)
              }
              capture.output(
              samples <- dic.fit.mcmc(dat=SI.Data, dist=SI.parametricDistr, init.pars = init.pars, burnin=burnin, n.samples=n1*thin, 
                           verbose=floor(total.samples.needed/100), seed=MCMCSeed)@samples
              , file=progressFile)
              file.remove(progressFile)
              if (.Platform$OS.type == "unix") {
                file.remove(pidFile)
              }
              return(samples)
            }))
          } else {
            if (method=="SIFromData") {
              # We have a full set of samples.
              mcmc_samples <- asyncData$mcmc_samples
              
              if (is.null(SI.Sample.From.Data)) {
                values$status <- "Running coarse2estim"
                startAsyncDataLoad("SI.Sample.From.Data", future({
                    coarse2estim(samples=mcmc_samples, dist=SI.parametricDistr, thin=thin)$SI.Sample
                }))
              } else if (is.null(convergenceCheck)) {
                values$status <- "Running the Gelman-Rubin convergence check"
                startAsyncDataLoad("convergenceCheck", future({
                  if (.Platform$OS.type == "unix") {
                    write(Sys.getpid(), file=pidFile)
                  }
                  ret <- check_CDTsamples_convergence(mcmc_samples)
                  if (.Platform$OS.type == "unix") {
                    file.remove(pidFile)
                  }
                  return(ret)
                }))
              } else {
                # Good to go!
                # Run SIFromSample not SIFromData using SI.Sample.From.Data (which is the result of us running MCMC)
                # The whole thing is equivalent to passing SI.Data to EstimateR(method="SIFromData"), but this way we
                # get a progress bar.
                if (!convergenceCheck) {
                  # FYI: This works in browsers, but seems to stop everything when done in RStudio
                  session$sendCustomMessage(type="alert", "Warning: The Gelan-Rubin algorithm suggests that MCMC may not have converged within the number of iterations sepcified (burnin + n1*thin).
                       EstimateR will be called anyway, but you should investigate this issue.")
                }
                values$status <- "Running EstimateR..."
                startAsyncDataLoad("epiEstimOutput", future({
                  if (.Platform$OS.type == "unix") {
                    write(Sys.getpid(), file=pidFile)
                  }
                  ret <- EstimateR(IncidenceData, T.Start, T.End, method="SIFromSample", n2=n2, SI.Sample=SI.Sample.From.Data, seed=seed,
                            Mean.Prior=Mean.Prior, Std.Prior=Std.Prior)
                  if (.Platform$OS.type == "unix") {
                    file.remove(pidFile)
                  }
                  return(ret)
                }))
             }
            } else {
              startAsyncDataLoad("epiEstimOutput", future({
                if (.Platform$OS.type == "unix") {
                  write(Sys.getpid(), file=pidFile)
                }
                ret <- EstimateR(IncidenceData, T.Start, T.End, method=method, n1=n1, n2=n2, Mean.SI = Mean.SI, Std.SI = Std.SI, 
                          Std.Mean.SI = Std.Mean.SI, Min.Mean.SI = Min.Mean.SI, Max.Mean.SI = Max.Mean.SI, Std.Std.SI = Std.Std.SI,
                          Min.Std.SI = Min.Std.SI, Max.Std.SI = Max.Std.SI, SI.Distr = SI.Distr, SI.Data = SI.Data, 
                          SI.Sample = SI.Sample, plot = plot, seed=seed, Mean.Prior=Mean.Prior, Std.Prior=Std.Prior)
                if (.Platform$OS.type == "unix") {
                  file.remove(pidFile)
                }
                return(ret)
              }))
            }
          }
          
        }
      },
      error = function (e) {
        show("prev")
        hide("stop")
        enable("go")
        handleError(values$state, e)
      })
  }
  
  output$plot <- renderPlot({
    if (!is.null(asyncData$epiEstimOutput)) {
      plots(asyncData$epiEstimOutput)
      values$status <- "Ready"
      show("prev")
      hide("stop")
      enable("go")
    }
  })
  
  output$savePlot <- downloadHandler(
    filename = function() {"Plot.png"},
    content = function(file) {
      if (!is.null(asyncData$epiEstimOutput)){
        png(file)
        print(plots(asyncData$epiEstimOutput))
        dev.off()
      }
    }
  )
  
  output$incidenceDataOutput <- renderTable({
    if (!is.null(asyncData$epiEstimOutput)) {
        local <- round(asyncData$epiEstimOutput$I_local, 2)
        imported <- round(asyncData$epiEstimOutput$I_imported, 2)
        values$status <- "Ready"
        show("prev")
        hide("stop")
        enable("go")
        data.frame(local, imported)
    }
  })
  
  output$saveIncidence <- downloadHandler(
    filename = function() {"IncidenceData.csv"},
    content = function(file) {
      if(!is.null(asyncData$epiEstimOutput)) {
        local <- asyncData$epiEstimOutput$I_local
        imported <- asyncData$epiEstimOutput$I_imported
        dat <- data.frame(local, imported)
        write.csv(dat, file, row.names=F)
      }
    }
  )
  
  output$saveSI <- downloadHandler(
    filename = function() {"SerialIntervalEstimates.csv"},
    content = function(file) {
      if(!is.null(asyncData$epiEstimOutput)) {
        write.csv(asyncData$epiEstimOutput$SI.Distr, file, row.names=F)
      }
    }
  )

  output$estimatedROutput <- renderTable({
    if (!is.null(asyncData$epiEstimOutput)) {
      values$status <- "Ready"
      show("prev")
      hide("stop")
      enable("go")
      round(asyncData$epiEstimOutput$R, 2)
    }
  })
  
  output$serialIntervalOutput <- renderTable({
    if (!is.null(asyncData$epiEstimOutput)) {
      values$status <- "Ready"
      show("prev")
      hide("stop")
      enable("go")
      round(asyncData$epiEstimOutput$SI.Distr, 2)
    }
  })
  
  output$saveR <- downloadHandler(
    filename = function() {"EstimatedR.csv"},
    content = function(file) {
      if(!is.null(asyncData$epiEstimOutput)) {
        write.csv(asyncData$epiEstimOutput$R, file, row.names=F)
      }
    }
  )
  
  
  handleState <- function() {
    # Run when next is clicked. Should handle all validation and error checks for that state
    # and should set all necessary variables.
    # The state will change only if handleState returns TRUE. 
    state <- values$state
    if (state %in% finalStates) {
      # Go (rather than next) was pressed.
      show("stop")
      disable("go")
      hide("prev")
      asyncData$epiEstimOutput <- NULL # Remove current plot
    }
    values$error <- NULL
    session$sendCustomMessage(type="resetErrorBoxes", "")
    values$status = "Processing..."
    tryCatch({
      switch(state,
             "1.1" = {
               TRUE
             },
             "2.1" = {
               # Handle uploaded data:
               IncidenceData <<- read.csv(input$incidenceData$datapath,
                                          header = input$incidenceHeader, sep = ",",
                                          quote = "")
               # Process Incidence data (see utils.R)
               IncidenceData <<- EpiEstim:::process_I(IncidenceData)
               length <- dim(IncidenceData)[1]
               W <- input$uploadedWidth
               if (W >= length) {
                 throwError("The width must be smaller than the length of your incidence data", "uploadedWidth")
                 throwError("The width must be smaller than the length of your incidence data", "incidenceData")
               }
               T.Start <<- 2:(length - W + 1)
               T.End <<- (1+W):length

               Mean.Prior <<- input$uploadedMeanPrior
               if (Mean.Prior < 0) {
                 throwError("Prior mean must be non-negative", "uploadedMeanPrior")
               }
               Std.Prior <<- input$uploadedStdPrior
               if (Std.Prior <=0) {
                 throwError("Prior standard deviation must be positive", "uploadedStdPrior")
               }
               TRUE
             },
             "2.2" = {
               # Get preloaded data
               IncidenceData <<- getIncidenceData(input$incidenceDataset)

               # Process Incidence data (using EpiEstim)
               IncidenceData <<- EpiEstim:::process_I(IncidenceData)
               
               length <- dim(IncidenceData)[1]
               W <- input$incidenceWidth
               if (W >= length) {
                 throwError("The width must be smaller than the length of your incidence data", "incidenceWidth", FALSE) # Don't stop until next one
                 throwError("The width must be smaller than the length of your incidence data", "incidenceData")
               }
               T.Start <<- 2:(length - W + 1)
               T.End <<- (1+W):length

               Mean.Prior <<- input$incidenceMeanPrior
               if (Mean.Prior < 0) {
                 throwError("Prior mean must be non-negative", "incidenceMeanPrior")
               }
               Std.Prior <<- input$incidenceStdPrior
               if (Std.Prior <=0) {
                 throwError("Prior standard deviation must be positive", "incidenceStdPrior")
               }
               TRUE
             },
             "3.1" = {TRUE},
             "4.1" = {
               IncidenceData <<- read.csv(input$incidenceData$datapath,
                                          header = input$incidenceHeader, sep = ",",
                                          quote = ",")
               ImportedData <- read.csv(input$importedData$datapath,
                                        header = input$importedHeader, sep = input$importedSep,
                                        quote = input$importedQuote)
               if (dim(ImportedData)[1] != dim(IncidenceData)[1]) {
                 # Lengths don't match
                 stop("The 'all cases' and the 'imported' datasets are not of the same length")
               }
               IncidenceData$imported <<- ImportedData[,1]
               colnames(IncidenceData) <<- c("local", "imported")
               
               # Currently the "local" column will be the total number of cases because of the way the app
               # is asking for inputs. Correct for this. 
               IncidenceData$local <<- IncidenceData$local - IncidenceData$imported
               
               # Process Incidence data (using EpiEstim)
               IncidenceData <<- EpiEstim:::process_I(IncidenceData)
               TRUE
             },
             "5.1" = {TRUE},
             "6.1" = {TRUE},
             "6.2" = {TRUE},
             "7.1" = {TRUE},
             "7.2" = {TRUE},
             "7.3" = {
               method <<- "UncertainSI"
               n1 <<- input$n1
               n2 <<- input$n2
               Mean.SI <<- input$Mean.SI
               Std.SI <<- input$Std.SI
               Std.Mean.SI <<- input$Std.Mean.SI
               Min.Mean.SI <<- input$Min.Mean.SI
               Max.Mean.SI <<- input$Max.Mean.SI
               Std.Std.SI <<- input$Std.Std.SI
               Min.Std.SI <<- input$Min.Std.SI
               Max.Std.SI <<- input$Max.Std.SI
               if (is.null(n1) || n1 < 1 || !is.integer(n1)) {
                 throwError("n1 must be an integer greater than or equal to 1", "n1")
               }
               if (is.null(n2) || n2 < 1 || !is.integer(n2)) {
                 throwError("n2 must be an integer greater than or equal to 1", "n2")
               }
               if (is.null(Mean.SI) || Mean.SI < 0) {
                 throwError("Mean.SI must be an greater than or equal to 0", "Mean.SI")
               }
               if (is.null(Min.Mean.SI) || Min.Mean.SI < 0) {
                 throwError("Std.SI must be an greater than or equal to 0", "Min.Mean.SI")
               }
               if (is.null(Max.Mean.SI) || Max.Mean.SI < 0) {
                 throwError("Std.SI must be an greater than or equal to 0", "Max.Mean.SI")
               }
               if (is.null(Std.Mean.SI) || Std.Mean.SI < 0) {
                 throwError("Std.SI must be an greater than or equal to 0", "Std.Mean.SI")
               }
               if (Min.Mean.SI > Mean.SI) {
                 throwError("Min.Mean.SI must be less than Mean.SI", "Min.Mean.SI", FALSE) # Don't stop until next one
                 throwError("Min.Mean.SI must be less than Mean.SI", "Mean.SI")
               }
               if (Mean.SI > Max.Mean.SI) {
                 throwError("Max.Mean.SI must be greater than Mean.SI", "Max.Mean.SI", FALSE) # Don't stop until next one
                 throwError("Max.Mean.SI must be greater than Mean.SI", "Mean.SI")
               }
               if (is.null(Std.SI) || Std.SI < 0) {
                 throwError("Std.SI must be an greater than or equal to 0", "Std.SI")
               }
               if (is.null(Min.Std.SI) || Min.Std.SI < 0) {
                 throwError("Std.SI must be an greater than or equal to 0", "Min.Std.SI")
               }
               if (is.null(Max.Std.SI) || Max.Std.SI < 0) {
                 throwError("Std.SI must be an greater than or equal to 0", "Max.Std.SI")
               }
               if (is.null(Std.Std.SI) || Std.Std.SI < 0) {
                 throwError("Std.SI must be an greater than or equal to 0", "Std.Std.SI")
               }
               if (Min.Std.SI > Std.SI) {
                 throwError("Min.Std.SI must be less than Std.SI", "Min.Std.SI", FALSE) # Don't stop until next one
                 throwError("Min.Std.SI must be less than Std.SI", "Std.SI")
               }
               if (Std.SI > Max.Std.SI) {
                 throwError("Max.Std.SI must be greater than Std.SI", "Max.Std.SI", FALSE) # Don't stop until next one
                 throwError("Max.Std.SI must be greater than Std.SI", "Std.SI")
               }

               requestedSeed <<- input$uncertainSeed
               if (!is.null(requestedSeed) & !is.na(requestedSeed)) {
                   # Actually set the seed now, to check it's valid
                   tryCatch({
                       set.seed(requestedSeed)
                   },
                   error = function(e) {
                       throwError("Invalid seed", "uncertainSeed")
                   })
               }
               TRUE
             },
             "7.4" = {
               Mean.SI <<- input$Mean.SI2
               Std.SI <<- input$Std.SI2
               method <<- "ParametricSI"
               if (is.null(Mean.SI) || Mean.SI < 0) {
                 throwError("Mean.SI must be an greater than or equal to 0", "Mean.SI2")
               }
               if (is.null(Std.SI) || Std.SI < 0) {
                 throwError("Std.SI must be an greater than or equal to 0", "Std.SI2")
               }
               TRUE
             },
             "7.5" = {
               method <<- "NonParametricSI"
               SI.Distr <<- as.numeric(read.csv(input$SIDistrData$datapath,
                                     header = input$SIDistrHeader, sep = ",",
                                     quote = ""))
               TRUE
             },
             "7.6" = {
               method <<- "NonParametricSI"
               SI.Distr <<- getSIDistribution(input$SIDistrDataset)
               TRUE
             },
             "8.1" = {
               # "SIFromSample"
               # Simply read the MCMC samples from the file. See getMCMCFit in utils.R
               method <<- "SIFromSample"
               SI.Sample <<- getSISamples(input$SIDataset, input$SIDist)

               requestedSeed <<- input$preloadedSeed
               if (!is.null(requestedSeed) & !is.na(requestedSeed)) {
                   # Actually set the seed now, to check it's valid
                   tryCatch({
                       set.seed(requestedSeed)
                   },
                   error = function(e) {
                       throwError("Invalid seed", "preloadedSeed")
                   })
               }

               n2 <<- input$n24
               if (is.null(n2) || n2 < 1 || !is.integer(n2)) {
                 throwError("n2 must be an integer greater than or equal to 1", "n24")
               }
               TRUE
             },
             "8.2" = {
               method <<- "SIFromData"
               serialIntervalData <- read.csv(input$SIData$datapath,
                                              header = input$SIHeader, sep = ",",
                                              quote = "")
               # Process the data (see function in utils.R)
               SI.Data <<- EpiEstim:::process_SI.Data(serialIntervalData)
               requestedSeed <<- input$uploadedSISeed
               if (!is.null(requestedSeed) & !is.na(requestedSeed)) {
                   # Actually set the seed now, to check it's valid
                   tryCatch({
                       set.seed(requestedSeed)
                   },
                   error = function(e) {
                       throwError("Invalid seed", "uploadedSISeed")
                   })

               }
               # Throw a warning about MCMC locking up app if only 1 core
               if (future::availableCores() == 1) {
                 alert(paste("WARNING:\n", "Your machine only has 1 core",
                             "available for EpiEstimApp to use. This means",
                             "that we cannot run MCMC in a separate process",
                             "which will cause the app to lock up while you run",
                             "MCMC. You may still run MCMC, however the app will",
                             "become completely unresponsive while MCMC is running,",
                             "which may take quite some time.", sep=""))
               }
               TRUE
             },
             "8.3" = {
               method <<- "SIFromSample"
               SI.Sample <<- EpiEstim:::process_SI.Sample(read.csv(input$SISampleData$datapath,
                                                       header = input$SISampleHeader, sep = ",",
                                                       quote = ""))
               n2 <<- input$n23
               if (is.null(n2) || n2 < 1 || !is.integer(n2)) {
                 throwError("n2 must be an integer greater than or equal to 1", "n23")
               }
               requestedSeed <<- input$SISampleSeed
               if (!is.null(requestedSeed) & !is.na(requestedSeed)) {
                   # Actually set the seed now, to check it's valid
                   tryCatch({
                       set.seed(requestedSeed)
                   },
                   error = function(e) {
                       throwError("Invalid seed", "SISampleSeed")
                   })
               }
               TRUE
             },
             "9.1" = {
               burnin <<- input$burnin
               total.samples.needed <<- input$burnin + input$n12 * input$thin
               n1 <<- input$n12
               n2 <<- input$n22
               thin <<- input$thin
               SI.parametricDistr <<- input$SIDist2
               mcmc_samples <<- asyncData$mcmc_samples
               SI.Sample.From.Data <<- asyncData$SI.Sample.From.Data
               convergenceCheck <<- asyncData$convergenceCheck
               if (!is.na(input$param1) && !is.na(input$param1)) {
                 init.pars <<- c(input$param1, input$param2)
               } else {
                 init.pars <<- init_MCMC_params(SI.Data, SI.parametricDistr)
               }

               if (is.null(n1) || n1 < 1 || !is.integer(n1)) {
                 throwError("n1 must be an integer greater than or equal to 1", "n12")
               }
               if (is.null(n2) || n2 < 1 || !is.integer(n2)) {
                 throwError("n2 must be an integer greater than or equal to 1", "n22")
               }
               if (is.null(thin) || thin < 1 || !is.integer(thin)) {
                 throwError("thin must be an integer greater than or equal to 1", "thin")
               }
               if (is.null(burnin) || burnin < 0 || !is.integer(burnin)) {
                 throwError("burnin must be a non-negative integer", "burnin")
               }

              requestedMCMCSeed <<- input$MCMCSeed
               if (!is.null(requestedMCMCSeed) & !is.na(requestedMCMCSeed)) {
                   # Actually set the seed now, to check it's valid
                   tryCatch({
                       set.seed(requestedMCMCSeed)
                   },
                   error = function(e) {
                       throwError("Invalid seed", "MCMCSeed")
                   })
               }
               # MCMC LIMITING:
               # We don't want too many of our cores invested in running MCMC.
               # If all cores are in use, the app will lock up for ALL USERS.
               # To deal with this, we will limit the number of cores used for
               # MCMC.
               cores <- future::availableCores()
               currentMCMC <- length(list.files(path=mcmcPidFolder))
               if (currentMCMC >= ceiling(cores/2)) {
                 alert(paste("ERROR: SERVER BUSY\n",
                      "Unfortunately the maximum number of MCMC processes are",
                      "already running on this server. This is probably because",
                      "other users are also running MCMC. Please try again later.\n",
                      "Alternatively, please install EpiEstimApp on your computer",
                      "and run your own instance.", sep=""))
                 values$status <- "SERVER BUSY"
                 hide("stop")
                 enable("go")
                 show("prev")
                 enable("prev")
                 FALSE
               } else {
                 TRUE
               }
             },
             stop(sprintf("An error occurred in handleState(). Input '%s' was not recognised.", state))
      )
    },
    error = function (e) {
      handleError(values$state, e)
      FALSE
    })
  } # End HandleState
  
  
  # getNextState and getPrevState encode the logic in the decision tree. 
  # See Decision Tree_Schematic.pdf in the root of this project.
  getNextState <- function (currentState) {
    switch(currentState,
           "1.1" = {if (input$incidenceDataType == "own") "2.1" else "2.2"},     
           "2.1" = {"3.1"},
           "2.2" = {"5.1"},
           "3.1" = {if (input$imported == "TRUE") "4.1" else "5.1"},
           "4.1" = {"5.1"},
           "5.1" = {if (input$SIPatientData == "TRUE") "6.1" else "6.2"},
           "6.1" = {if (input$SIDataType == "preloaded") "7.1" else "7.2"},
           "6.2" = {
             if (input$SIEstType == "uncertain") {
               "7.3"
             } else if (input$SIEstType == "parametric") {
               "7.4"
             } else if (input$SIEstType == "own") {
               "7.5"
             } else if (input$SIEstType == "preloaded") {
               "7.6"
             } else {
               stop("Error in getNextState(), SIEstType not found")
             }
           },
           "7.1" = {"8.1"},
           "7.2" = {if (input$SIFrom == "data") "8.2" else "8.3"},
           "8.2" = {"9.1"},
           stop(sprintf("An error occurred in getNextState(). Input '%s' was not recognised.", currentState))
    )
  }
  
  getPrevState <- function (currentState) {
    switch(currentState,
           "2.1" = {"1.1"},
           "2.2" = {"1.1"},
           "3.1" = {"2.1"},
           "4.1" = {"3.1"},
           "5.1" = {
             if (input$incidenceDataType == "own") {
               if (input$imported == "TRUE") "4.1" else "3.1"
             } else {
               "2.2"
             }
           },
           "6.1" = {"5.1"},
           "6.2" = {"5.1"},
           "7.1" = {"6.1"},
           "7.2" = {"6.1"},
           "7.3" = {"6.2"},
           "7.4" = {"6.2"},
           "7.5" = {"6.2"},
           "7.6" = {"6.2"},
           "8.1" = {"7.1"},
           "8.2" = {"7.2"},
           "8.3" = {"7.2"},
           "9.1" = {"8.2"},
           stop(sprintf("An error occurred in getPrevState(). Input '%s' was not recognised.", currentState))
    )
  }
  
  ### The following is to make everything as asyncronous as possible to prevent slow functions being blocking.
  startAsyncDataLoad <- function(asyncDataName, futureObj) {
    checkAsyncDataBeingLoaded$suspend()
    asyncDataBeingLoaded[[asyncDataName]] <<- futureObj
    checkAsyncDataBeingLoaded$resume()
  } #end startAsyncDataLoad
  
  checkAsyncDataBeingLoaded <- observe({
    invalidateLater(1000)
    for (asyncDataName in names(asyncDataBeingLoaded)) {
      asyncFutureObject <- asyncDataBeingLoaded[[asyncDataName]]
      if (resolved(asyncFutureObject)) {
        tryCatch({
          asyncData[[asyncDataName]] <<- value(asyncFutureObject)
          asyncDataBeingLoaded[[asyncDataName]] <<- NULL
          
          # If we've resolved something but asyncData$epiEstimOutput is not loaded then we've been
          # incrementally running MCMC and are not done yet. We want to re-start stuff, so call run() again
          if (is.null(asyncData$epiEstimOutput)) {
            run()
          }
        },
        error = function (e) {
          checkAsyncDataBeingLoaded$suspend() # Stop running, otherwise we'll throw the error every 1000ms.
          handleError(values$state, e)
          FALSE
        })
      }
    }#end loop over async data items being loaded
    #if there are no more asynchronous data items being loaded then stop checking
    if (length(asyncDataBeingLoaded) == 0) {
      checkAsyncDataBeingLoaded$suspend()
    }
    
    # If MCMC is being run, we should check on progress.
    if (method == "SIFromData") {
      prog <- getMCMCProgress(progressFile)
      if (prog > 0 & prog < total.samples.needed) {
        values$status <- paste("Running MCMC (", floor(100*prog/total.samples.needed), "%)", sep="")
      }
    }
  }, suspended = TRUE) # checkAsyncDataBeingLoaded
  
  handleError <- function(state, error) {
    #stop(error) #Uncomment in dev for detailed stack trace etc
    if (error$message == "handled") {
      # We've properly handled an error, and have used `stop("handled")` to stop the app. 
      # Nothing should be done here in this case. 
      return()
    }
    values$status <- "ERROR"
    cat("There was an error in state", state, "\n")
    cat(error$message, "\n")
    enable("go")
    hide("stop")
    show("prev")
    switch(state,
           "2.1" = {
             if (error$message == "'file' must be a character string or connection") {
               throwError("Please upload a file", "incidenceData", FALSE)
             } else {
               info(error$message)
             }
           },
           "8.1" = {
             if (error$message == "The Rotavirus dataset has serial intervals which are definitely less than 1, so an offset distribution is not appropriate."){
               throwError("The Rotavirus dataset has serial intervals which are definitely less than 1, so an offset distribution is not appropriate. Please use a different SI distribution, or change your dataset", "SIDist", FALSE)
             } else {
               info(error$message)
             }
           },
           info(error$message) # Fallback to JS alert
    )
    return()
  }
  
  throwError <- function(errorMessage, errorBoxName = NULL, error=TRUE) {
    # Throws an error nicely. If you want to highlight a specific input in red, give the id
    # of that input (found in ui.R) as errorBoxName. The errorMessage will be displayed 
    # as some red text.
    # The error argument should be set to false only by the handleError function above.
    # Otherwise it should be set to true to stop execution (which will be happening inside a tryCatch)
    if (!is.null(errorBoxName)) {
      session$sendCustomMessage(type="errorBox", errorBoxName)
    }
    values$error <- errorMessage
    enable("go")
    hide("stop")
    show("prev")
    # Throw an error to actually stop the app, but say we've handled telling the client about the problem.
    if (error) {
      stop("handled")
    }
  }
  
  session$onSessionEnded(function() {
    checkAsyncDataBeingLoaded$suspend()
    if(file.exists(pidFile)) {
      # Something async is running (on unix), kill it.
      pid <- read.csv(pidFile, header=FALSE)
      tools::pskill(pid)
      file.remove(pidFile)
    }
    if(file.exists(progressFile)) {
      file.remove(progressFile)
    }
  })
  
  observeEvent(input$stop, {
    checkAsyncDataBeingLoaded$suspend()
    if(file.exists(pidFile)) {
      # Something async is running (on unix), kill it.
      pid <- read.csv(pidFile, header=FALSE)
      tools::pskill(pid)
      file.remove(pidFile)
    }
    if(file.exists(progressFile)) {
      file.remove(progressFile)
    }
    hide("stop")
    enable("go")
    show("prev")
    values$status <- "Ready"
  })
  
  observe({
    # This function removes the asyncData$mcmc_samples whenever the corresponding inputs change. 
    input$SIData
    input$SISep
    input$SIHeader
    input$SIQuote
    input$SIDist2
    input$param1
    input$param2
    input$n12
    input$n22
    input$burnin
    input$thin
    
    asyncData$mcmc_samples <<- NULL
    mcmc_samples <<- NULL
    asyncData$SI.Sample.From.Data <<- NULL
    SI.Sample.From.Data <<- NULL
    asyncData$convergenceCheck <<- NULL
    convergenceCheck <<- NULL
  })
  
}) # End shinyServer

