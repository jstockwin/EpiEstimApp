library(devtools)
install_github("robin-thompson/coarseDataTools", ref = "hackout3")
library(coarseDataTools)
library(MCMCpack)
install_github('annecori/EpiEstim', ref = "hackout3")
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

data(Measles1861)
data(Flu1918)
data(Smallpox1972)
data(SARS2003)
data(Flu2009)
alldatasets <- list('Measles1861' = Measles1861,
                    'Flu1918' = Flu1918,
                    'Smallpox1972' = Smallpox1972,
                    'SARS2003' = SARS2003,
                    'Flu2009' = Flu2009)


# Source necessary files
source("stochasticSEIRModel3.R", local=TRUE)
source("utils.R", local=TRUE)
#source("estimater.R", local=TRUE)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)
options(shiny.reactlog=TRUE) 

allStates = c("1.1", "2.1", "2.2", "3.1", "4.1", "5.1", "6.1", "6.2", "7.1", "7.2", "7.3", "7.4",
              "8.1", "8.2", "8.3", "8.4", "8.5", "9.1", "9.2", "9.3")

finalStates = c("8.1", "9.1", "8.3", "7.3", "8.4", "9.2", "9.3")

shinyServer(function(input, output, session) {
  # DEBUG
  #observe({
  #  cat(values$state, "\n")
  #})
  
  
  # Initialise some reactive values
  t <- as.numeric(Sys.time())
  id <- 1e8 * (t - floor(t))
  id <- gsub("\\.", "-", as.character(id))
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
  
  output$error <- renderText({values$error})
  
  
  
  # Logic for when "go" is clicked.
  observeEvent(input$go, {run()})
  
  run <- function() {
      # WARNING: You probably want to avoid much logic here. Most of it should be in handleState() which is reactive. 
      # If Next is pressed twice without inputs changing, nothing will happen, but if anything you put here WILL get done.
      tryCatch({
        if (handleState()) {
          if (method=="SIFromData" && is.null(mcmc_samples)) {
            values$status <- "Running MCMC (0%)"
            startAsyncDataLoad("mcmc_samples", future({
              capture.output(
              samples <- dic.fit.mcmc(dat=SI.Data, dist=SI.parametricDistr, init.pars = init.pars, burnin=burnin, n.samples=n1*thin, 
                           verbose=floor(total.samples.needed/100))@samples
              , file=paste("progress/", id, "-progress.txt", sep=""))
              file.remove(paste("progress/", id, "-progress.txt", sep=""))
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
                  check_CDTsamples_convergence(mcmc_samples)
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
                  EstimateR(IncidenceData, T.Start, T.End, method="SIFromSample", n2=n2, SI.Sample=SI.Sample.From.Data)
                }))
              }
            } else {
              startAsyncDataLoad("epiEstimOutput", future({
                EstimateR(IncidenceData, T.Start, T.End, method=method, n1=n1, n2=n2, Mean.SI = Mean.SI, Std.SI = Std.SI, 
                          Std.Mean.SI = Std.Mean.SI, Min.Mean.SI = Min.Mean.SI, Max.Mean.SI = Max.Mean.SI, Std.Std.SI = Std.Std.SI,
                          Min.Std.SI = Min.Std.SI, Max.Std.SI = Max.Std.SI, SI.Distr = SI.Distr, SI.Data = SI.Data, 
                          SI.Sample = SI.Sample, plot = plot)
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
  
  
  handleState <- reactive({
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
             "1.1" = {TRUE},
             "2.1" = {
               # Handle uploaded data:
               IncidenceData <<- read.csv(input$incidenceData$datapath, 
                                          header = input$incidenceHeader, sep = input$incidenceSep,
                                          quote = input$incidenceQuote)
               # Process Incidence data (see utils.R)
               IncidenceData <<- processIncidenceData(IncidenceData)
               
               length <- dim(IncidenceData)[1]
               W <- input$uploadedWidth
               T.Start <<- 1:(length - W)
               T.End <<- (1+W):length
               TRUE
             },
             "2.2" = {
               # Get preloaded data
               IncidenceData <<- getIncidenceData(input$incidenceDataset, alldatasets)
               
               # Process Incidence data (see utils.R)
               IncidenceData <<- processIncidenceData(IncidenceData)
               
               length <- dim(IncidenceData)[1]
               W <- input$uploadedWidth
               T.Start <<- 1:(length - W)
               T.End <<- (1+W):length
               TRUE
             },
             "3.1" = {TRUE},
             "4.1" = {
               ImportedData <- read.csv(input$importedData$datapath,
                                        header = input$importedHeader, sep = input$importedSep,
                                        quote = input$importedQuote)
               # Process Incidence data (see utils.R) (IncidenceData will have been handled in state 2.1)
               IncidenceData <- processIncidenceData(IncidenceData, ImportedData)
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
               TRUE
             },
             "7.4" = {TRUE},
             "8.1" = {
               # "SIFromSample"
               # Simply read the MCMC samples from the file. See getMCMCFit in utils.R 
               method <<- "SIFromSample"
               SI.Sample <<- getSISamples(input$SIDataset, input$SIDist)
               TRUE
             },
             "8.2" = {
               method <<- "SIFromData"
               serialIntervalData <- read.csv(input$SIData$datapath, 
                                              header = input$SIHeader, sep = input$SISep,
                                              quote = input$SIQuote)
               # Process the data (see function in utils.R)
               SI.Data <<- processSerialIntervalData(serialIntervalData)
               TRUE
             },
             "8.3" = {
               SI.Sample <<- processSISamples(read.csv(input$SISampleData$datapath, 
                                                       header = input$SISampleHeader, sep = input$SISampleSep,
                                                       quote = input$SISampleQuote))
               n2 <<- input$n23
               TRUE
             },
             "8.4" = {
               Mean.SI <<- input$Mean.SI2
               Std.SI <<- input$Std.SI2
               method <<- "ParametricSI"
               TRUE
             },
             "8.5" = {TRUE},
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
               TRUE
             },
             "9.2" = {
               method <<- "NonParametricSI"
               SI.Distr <<- read.csv(input$SIDistrData$datapath, 
                                     header = input$SIDistrHeader, sep = input$SIDistrSep,
                                     quote = input$SIDistrQuote)
               TRUE
             },
             "9.3" = {
               method <<- "NonParametricSI"
               SI.Distr <<- alldatasets[[input$SIDistrDataset]]$SI.Distr
               TRUE
             },
             stop(sprintf("An error occurred in handleState(). Input '%s' was not recognised.", state))
      )
    },
    error = function (e) {
      handleError(values$state, e)
      FALSE
    })
  })
  
  
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
           "6.2" = {if (input$uncertainty == "TRUE") "7.3" else "7.4"},
           "7.1" = {"8.1"},
           "7.2" = {if (input$SIFrom == "data") "8.2" else "8.3"},
           "7.4" = {if (input$parametric == "TRUE") "8.4" else "8.5"},
           "8.2" = {"9.1"},
           "8.5"= {if (input$SIDistrDataType == "own") "9.2" else "9.3"},
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
           "8.1" = {"7.1"},
           "8.2" = {"7.2"},
           "8.3" = {"7.2"},
           "8.4" = {"7.4"},
           "8.5" = {"7.4"},
           "9.1" = {"8.2"},
           "9.2" = {"8.5"},
           "9.3" = {"8.5"},
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
      prog <- getMCMCProgress(paste("progress/", id, "-progress.txt", sep=""))
      if (prog > 0 & prog < total.samples.needed) {
        values$status <- paste("Running MCMC (", floor(100*prog/total.samples.needed), "%)", sep="")
      }
    }
  }, suspended = TRUE) # checkAsyncDataBeingLoaded
  
  handleError <- function(state, error) {
    #stop(error) #Uncomment in dev for detailed stack trace etc
    values$status <- "ERROR"
    cat("There was an error in state", state, "\n")
    cat(error$message, "\n")
    enable("go")
    hide("stop")
    show("prev")
    switch(state,
           "2.1" = {
             if (error$message == "'file' must be a character string or connection") {
               session$sendCustomMessage(type="errorBox", "incidenceData")
               values$error <- "Please upload a file!"
             } else {
               info(error$message)
             }
           },
           "8.1" = {
             if (error$message == "The Rotavirus dataset has serial intervals which are definitely less than 1, so an offset distribution is not appropriate."){
               session$sendCustomMessage(type="errorBox", "SIDist")
               values$error <- "The Rotavirus dataset has serial intervals which are definitely less than 1, so an offset distribution is not appropriate. Please use a different SI distribution, or change your dataset"
             } else {
               info(error$message)
             }
           },
           info(error$message) # Fallback to JS alert
    )
    return()
  }
  
  session$onSessionEnded(function() {
    checkAsyncDataBeingLoaded$suspend()
  })
  
  observeEvent(input$stop, {
    checkAsyncDataBeingLoaded$suspend()
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

