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
  # Initialise some reactive values
  values <- reactiveValues(state="1.1", status="Ready", epiEstimOutput = NULL)
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
    }
  })
  observeEvent(input$prev, {
    values$state = getPrevState(values$state)
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
  
  
  
  # Logic for when "go" is clicked.
  observeEvent(input$go, {
    #tryCatch({
      if (handleState()) {
        values$status = "Running"
        startAsyncDataLoad("epiEstimOutput", future({
          EstimateR_func # Workaround ref: https://github.com/HenrikBengtsson/future/issues/137
          EstimateR(IncidenceData, T.Start, T.End, method=method, n1=n1, n2=n2, Mean.SI = Mean.SI, Std.SI = Std.SI, 
                                                              Std.Mean.SI = Std.Mean.SI, Min.Mean.SI = Min.Mean.SI, Max.Mean.SI = Max.Mean.SI, Std.Std.SI = Std.Std.SI,
                                                              Min.Std.SI = Min.Std.SI, Max.Std.SI = Max.Std.SI, SI.Distr = SI.Distr, SI.Data = SI.Data, 
                                                              SI.parametricDistr = SI.parametricDistr, MCMC.control = MCMC.control, SI.Sample = SI.Sample, plot = plot)
          }))
      }
    #},
    #error = function (e) {
    #  ## TODO: Graciously handle as many expected errors as possible. 
    #  ## Perhaps make a new function handleErrors(error, state)?
    #  info(e$message) ## TODO: This is just a JS alert for now. Should be done better.
    #  error(e)
    #})
  })
  
  output$plot <- renderPlot({
    if (!is.null(asyncData[["epiEstimOutput"]])) {
      p_I <- plots(asyncData[["epiEstimOutput"]], what="I")
      #p_SI <- plots(values$epiEstimOutput, what="SI")
      p_R <- plots(asyncData[["epiEstimOutput"]], what="R")
      gridExtra::grid.arrange(p_I,p_R,ncol=1)
    }
  })
  
  
  handleState <- reactive({
    state <- values$state
    # Run when next is clicked. Should handle all validation and error checks for that state
    # and should set all necessary variables.
    # The state will change only if handleState returns TRUE. 
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
             
             length <- dim(IncidenceData)[1]
             W <- input$uploadedWidth
             T.Start <<- 1:(length - W)
             T.End <<- (1+W):length
             TRUE
           },
           "5.1" = {TRUE},
           "6.1" = {TRUE},
           "7.1" = {TRUE},
           "8.1" = {
             # "SIFromSample"
             # Simply read the MCMC samples from the file. See getMCMCFit in utils.R 
             method <<- "SIFromSample"
             SI.Sample <<- getSISamples(input$SIDataset, input$SIDist)
             TRUE
           },
           stop(sprintf("An error occurred in handleState(). Input '%s' was not recognised.", state))
     )
    },
    error = function (e) {
      ## TODO: Graciously handle as many expected errors as possible. 
      ## Perhaps make a new function handleErrors(error, state)?
      info(e$message) ## TODO: This is just a JS alert for now. Should be done better.
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
           "3.1" = {"4.1"},
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
        asyncData[[asyncDataName]] <<- value(asyncFutureObject)
        asyncDataBeingLoaded[[asyncDataName]] <<- NULL
      }
    }#end loop over async data items being loaded
    #if there are no more asynchronous data items being loaded then stop checking
    if (length(asyncDataBeingLoaded) == 0) {
      checkAsyncDataBeingLoaded$suspend()
    }
  }, suspended = TRUE) # checkAsyncDataBeingLoaded
  
}) # End shinyServer
