library(devtools)
install_github("nickreich/coarseDataTools", ref = "hackout3")
library(coarseDataTools)
library(MCMCpack)
install_github('annecori/EpiEstim', ref = "hackout3")
library(EpiEstim)
library(shiny)
library(shinyjs)
library(rjson)
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


# Part of workaround below ref: https://github.com/HenrikBengtsson/future/issues/137
# The below functions aren't in the workspace at all. We'll add them like this so
# they can then be mentioned in the future to make them globals, so they can be used asyncronously.
process_SI.Data <- EpiEstim:::process_SI.Data
check_SI.Distr <- EpiEstim:::check_SI.Distr
### end part of workaround


# Source necessary files
source("dic.fit.mcmc.incremental.R", local=TRUE)
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
  observe({
    cat(values$state, "\n")
  })
  
  
  # Initialise some reactive values
  values <- reactiveValues(state="1.1", status="Ready", error = NULL)
  enable("nxt") # Enable next button when initial load is done.
  
  asyncData <-
    reactiveValues(epiEstimOutput = NULL)
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
  MCMC.control = list(init.pars = NULL, burnin = 3000, thin=10, seed = NULL)
  SI.Sample = NULL
  plot = FALSE
  
  
  # Clicking previous/next should increment the stateLevel
  observeEvent(input$nxt, {
    if (handleState()) {
      values$state = getNextState(values$state)
      values$status = "Ready"
    }
  })
  observeEvent(input$prev, {
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
  observeEvent(input$go, {
    asyncData$epiEstimOutput <- NULL # Remove current plot
    tryCatch({
      if (handleState()) {
        startAsyncDataLoad("epiEstimOutput", future({
          ##########################
          # # Workaround ref: https://github.com/HenrikBengtsson/future/issues/137
          # # (Make certain things globals)
          EstimateR_func 
          OverallInfectivity
          process_SI.Data 
          .Random.seed
          dic.fit.mcmc
          check_SI.Distr
          # # End workaround
          ##########################
          EstimateR(IncidenceData, T.Start, T.End, method=method, n1=n1, n2=n2, Mean.SI = Mean.SI, Std.SI = Std.SI, 
                    Std.Mean.SI = Std.Mean.SI, Min.Mean.SI = Min.Mean.SI, Max.Mean.SI = Max.Mean.SI, Std.Std.SI = Std.Std.SI,
                    Min.Std.SI = Min.Std.SI, Max.Std.SI = Max.Std.SI, SI.Distr = SI.Distr, SI.Data = SI.Data, 
                    SI.parametricDistr = SI.parametricDistr, MCMC.control = MCMC.control, SI.Sample = SI.Sample, plot = plot)
        }))
      }
    },
    error = function (e) {
      handleError(values$state, e)
    })
  })
  
  output$plot <- renderPlot({
    if (!is.null(asyncData$epiEstimOutput)) {
      p_I <- plots(asyncData$epiEstimOutput, what="I")
      #p_SI <- plots(asyncData$epiEstimOutput, what="SI")
      p_R <- plots(asyncData$epiEstimOutput, what="R")
      gridExtra::grid.arrange(p_I,p_R,ncol=1)
      values$status <- "Ready"
    }
  })
  
  
  handleState <- reactive({
    # Run when next is clicked. Should handle all validation and error checks for that state
    # and should set all necessary variables.
    # The state will change only if handleState returns TRUE. 
    state <- values$state
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
               MCMC.control$burnin <<- input$burnin
               MCMC.control$thin <<- input$thin
               if (!is.na(input$param1) && !is.na(input$param1)) {
                 MCMC.control$init.pars <<- c(input$param1, input$param2)
               }
               n1 <<- input$n12
               n2 <<- input$n22
               SI.parametricDistr <<- input$SIDist2
               MCMC.control$seed <<- 1 # TODO: REMOVE THIS! There is an issue with EpiEstim's default behaviour right now.
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
  }, suspended = TRUE) # checkAsyncDataBeingLoaded
  
  handleError <- function(state, error) {
    values$status <- "ERROR"
    cat("There was an error in state", state, "\n")
    cat(error$message, "\n")
    switch(state,
           "2.1" = {
             if (error$message == "'file' must be a character string or connection") {
               session$sendCustomMessage(type="errorBox", "incidenceData")
               values$error <- "Please upload a file!"
             }
           },
           info(error$message)
    )
    return()
  }
  
}) # End shinyServer

