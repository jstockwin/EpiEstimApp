library(devtools)
install_github("nickreich/coarseDataTools", ref = "hackout3")
library(coarseDataTools)
library(MCMCpack)
install_github('annecori/EpiEstim', ref = "hackout3")
library(EpiEstim)
library(shiny)
library(rjson)
library(ggplot2)
library(graphics)
library(grid)
library(gridExtra)
library(plotly)
library(plyr)
library(reshape2)
library(stats)

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

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)
options(shiny.reactlog=TRUE) 



shinyServer(function(input, output, session) {
   
  output$plot <- renderPlot({
    input$status
    if (is.null(input$status) || input$status == "STOP") {
      # Tell client we're 'done' to say we're ready.
      session$sendCustomMessage(type='done', "")
      return()
    }

    # Put everything in a try catch, as we need to send the "DONE" message to the client
    # even if we exit with an error.
    tryCatch({
      isolate({
        
        # Work out which state we're in
        SIState <- getSIState(input)
        incidenceState <- getIncidenceState(input)
        
        ####################
        ## Incidence Data ##
        ####################
        
        if (incidenceState == 2.1) {
          # Handle uploaded data:
          IncidenceData <- read.csv(input$incidenceData$datapath, 
                                      header = input$incidenceHeader, sep = input$incidenceSep,
                                      quote = input$incidenceQuote)
          # Process Incidence data (see utils.R)
          IncidenceData <- processIncidenceData(IncidenceData)
        } else if (incidenceState == 2.2) {
          # Get preloaded data
          IncidenceData <- getIncidenceData(input$incidenceDataset, alldatasets)
        }
       
        #######################
        ## Deal with SI Data ##
        #######################
        
        if (SIState == 6.1) {
          # "NonParametricUncertainSI"
          # Simply read the MCMC samples from the file. See getMCMCFit in utils.R 
          MCMC <- getMCMCFit(input$SIDataset, input$SIDist)
          ####  FEED INTO EPIESTIM
          W <- input$Width
          length <- dim(IncidenceData)[1]
          session$sendCustomMessage(type='updateStatus', "Running EstimateR...")
          EstimateR(IncidenceData[,2], T.Start=1:(length - W), T.End=(1+W):length, n2 = dim(MCMC@samples)[2], CDT = MCMC, plot=TRUE)
          session$sendCustomMessage(type='done', "")
          
        } else if (SIState == 6.2) {
          # "NonParametricUncertainSI"
          # Uploaded data, need to run MCMC. Run the next 80 iterations.
          MCMC = run_MCMC()
          tryCatch({
            # Trycatch becase ==FALSE fails if MCMC is actually an MCMC fit...
            if (MCMC == FALSE) {
              # run_MCMC has pinged data back to the client. We should return here, and instead handle the incoming data.
              return(NULL)
            }
          }, error = function (e) {# Ignore the error.
            })

          # else we actually have an MCMC fit.
          
          # If we reach here, we're done with MCMC
          MCMC@samples <- MCMC@samples[3000:8000,] #Remove burnin
          ####  FEED INTO EPIESTIM
          W <- input$Width
          length <- dim(IncidenceData)[1]
          session$sendCustomMessage(type='updateStatus', "Running EstimateR...")
          EstimateR(IncidenceData[,2], T.Start=1:(length - W), T.End=(1+W):length, n2 = dim(MCMC@samples)[2], CDT = MCMC, plot=TRUE)
          session$sendCustomMessage(type='done', "")
        } else if (SIState == 6.3) {
          # "UncertainSI"
          ####  FEED INTO EPIESTIM
          W <- input$Width
          length <- dim(IncidenceData)[1]
          session$sendCustomMessage(type='updateStatus', "Running EstimateR...")
          EstimateR(IncidenceData[,2], T.Start=1:(length - W), T.End=(1+W):length, method="UncertainSI", n1=input$n1, n2=input$n2,
                    Mean.SI=input$Mean.SI, Std.SI=input$Std.SI,
                    Std.Mean.SI=input$Std.Mean.SI, Min.Mean.SI=input$Min.Mean.SI, Max.Mean.SI=input$Max.Mean.SI, 
                    Std.Std.SI=input$Std.Std.SI, Min.Std.SI=input$Min.Std.SI, Max.Std.SI=input$Max.Std.SI, plot=TRUE)
          session$sendCustomMessage(type='done', "")
        } else if (SIState == 6.4) {
          # "ParametricSI"
          ####  FEED INTO EPIESTIM
          W <- input$Width
          length <- dim(IncidenceData)[1]
          session$sendCustomMessage(type='updateStatus', "Running EstimateR...")
          EstimateR(IncidenceData[,2], T.Start=1:(length - W), T.End=(1+W):length, Mean.SI=input$Mean.SI2, Std.SI=input$Std.SI2,
                    method="ParametricSI", plot=TRUE)
          session$sendCustomMessage(type='done', "")
        } else if (SIState == 6.5) {
          # "NonParametricSI"
          ####  FEED INTO EPIESTIM
          W <- input$Width
          length <- dim(IncidenceData)[1]
          if (input$SIDistrDataset == 'Uploaded Data') {
            SI.Distr = read.csv(input$SIDistrData$datapath, 
                                header = input$SIDistrHeader, sep = input$SIDistrSep,
                                quote = input$SIDistrQuote)
          } else {
            SI.Distr = alldatasets[[input$SIDistrDataset]]$SI.Distr
          }
          session$sendCustomMessage(type='updateStatus', "Running EstimateR...")
          EstimateR(IncidenceData[,2], T.Start=1:(length - W), T.End=(1+W):length, method='NonParametricSI', SI.Distr=SI.Distr, plot=TRUE)
          session$sendCustomMessage(type='done', "")
        }
      }) # End Isolate
    },
    error = function (e) {
      # Send message to client that we're done.
      session$sendCustomMessage(type='updateStatus', "ERROR")
      session$sendCustomMessage(type='done', "")
      stop(e)
    }) # End tryCatch
    
  }) # End output$plot
  
  # Calculating fit takes a long time. We'll make it reactive
  # so that it only updated when a new serialIntervalDataFile is supplied.
  run_MCMC <- function() {

    serialIntervalData <- read.csv(input$SIData$datapath, 
                                   header = input$SIHeader, sep = input$SISep,
                                   quote = input$SIQuote)
    
    # Process the data (see function in utils.R)
    serialIntervalData <- processSerialIntervalData(serialIntervalData)
    
    # If the distribution is set to offset gamma, we should check this is reasonable.
    if (input$SIDist2 == 'off1G' && any(serialIntervalData[4] - serialIntervalData[1] < 1)) {
      stop('The chosen dataset has serial intervals which are definitely less than 1,
             so a gamma distribution offset by 1 is not appropriate.')
    }
    
    if (is.na(input$param1) || is.na(input$param1)) {
      params = c(1,1)
    } else {
      params = c(input$param1, input$param2)
    }
    
    if (is.null(input$mydata) || input$mydata == "NEW") {
      # New run requested
      # Update the client about the current state for which MCMC is being ran for:
      session$sendCustomMessage(type='setMCMCInfo', paste('["', paste(URLencode(input$SIData$datapath), input$SIDist2, input$param1, input$param2, sep='","'), '"]', sep=''))
      
      

      session$sendCustomMessage(type='updateStatus', "Running MCMC... 0%")
      MCMC = dic.fit.mcmc.incremental(dat = serialIntervalData, dist=input$SIDist2,
                                      init.pars = params, increment.size = 80)
      # We're not yet done, so ping data back to client to check if we should continue
      data <- toJSON(MCMC@samples)
      session$sendCustomMessage(type='pingToClient', data) 
      return(FALSE)
    } else {
      # We have samples from the client, which are from a previous (potentially partial) run.
      
      dataChanged = checkIfSIDataUpdated(input)
      
      if (dataChanged) {
        # Tell the client to try again with mydata = "NEW", meaning MCMC will start again in the above code.
        session$sendCustomMessage(type='pingToClient', "NEW") 
        return(FALSE)
      }
      # If this code has reached then the inputs haven't changed and we should either
      # continue running MCMC or, if we've got a full set of data, simply return the
      # MCMC object for that data.
      
      current = as.data.frame(fromJSON(input$mydata))
      session$sendCustomMessage(type='updateStatus', paste('Running MCMC... ', round(dim(current)[1]*100/8000), '%', sep=''))
  
      if(dim(current)[1] < 8000) {
        # A partial run has been completed. Let's continue.

        MCMC = dic.fit.mcmc.incremental(dat = serialIntervalData, dist=input$SIDist2,
                                 current.samples = current, increment.size = 80)
        data <- toJSON(MCMC@samples)
        session$sendCustomMessage(type='pingToClient', data)
        return(FALSE)
        
      } else {
        # A full set of data has been provided for the correct inputs.
        # This will only happen on a re-run.


        # We'd like to just use current, but it needs to be in an MCMC object.
        # We'll simply add another few iterations onto the end to get the MCMC object around it. 
        #
        # TODO ACORI is going to update estimateR/coarse2estim to take samples as an argument rather
        # than MCMC. This will mean we don't have to do this, as we can just return(current) here.
        MCMC = dic.fit.mcmc.incremental(dat = serialIntervalData, dist=input$SIDist2,
                                        current.samples = current, increment.size = 2, n.samples = dim(current)[1] - 3000 + 2)
        session$sendCustomMessage(type='updateStatus', "MCMC Finished. Processing...")
        return(MCMC)
      }
      
      
    }
    
    
  } # End get_uploaded_fit
  
}) # End shinyServer


checkIfSIDataUpdated <- function (input) {
  if(is.null(input$oldSIDatapath)) {
    return (TRUE)
  }
  
  if (is.na(input$param1) && input$oldParam1 != "NA") {
    # param1 is now NA and wasn't before. Something has changed.
    return(TRUE)
  }
  if (is.na(input$param2) && input$oldParam2 != "NA") {
    # param2 is now NA and wasn't before. Something has changed.
    return(TRUE)
  }
  if (!is.na(input$param1) && input$param1 != input$oldParam1) {
    # param1 is not NA, but has changed.
    return(TRUE)
  }
  if (!is.na(input$param2) && input$param2 != input$oldParam2) {
    # param2 is not NA, but has changed.
    return(TRUE)
  }
  if (URLencode(input$SIData$datapath) != input$oldSIDatapath || input$SIDist2 != input$oldSIDist) {
    # The inputs have changed since MCMC was last run, so we can't keep the old fit.
    return(TRUE)
  }
  return(FALSE)
}


getSIState <- function (input) {
  if (input$SIPatientData) {
    # State 4.1
    if (input$SIDataType == 'preloaded') {
      # State 5.1
      if (!is.null(input$SIDataset) && !is.null(input$SIDist)) {
        # State 6.1
        return(6.1)
      } else {
        stop('Error: Invalid State (1). This should never happen, something went wrong.')
      }
    } else if (input$SIDataType == 'own') {
      # State 5.2
      if (!is.null(input$SIData) & !is.null(input$SIDist) & !is.null(input$param1) & !is.null(input$param2)) {
        # State 6.2
          return(6.2)
      } else {
        stop('Error: Invalid State (2). This should never happen, something went wrong.')
      }
    } else {
      stop('Error: Invalid State (3). This should never happen, something went wrong.')
    }
  } else {
    if (!is.null(input$SIPatientData)) {
      # State 4.2
      if (input$uncertainty) {
        # State 5.3
        # State 6.3
        return(6.3)
      } else {
        # State 5.4
        if (input$parametric) {
          # State 6.4
          return(6.4)
        } else {
          # State 6.5
          return(6.5)
        }
      }
    }
  }
  stop('Error: Invalid State (5). This should never happen, something went wrong.')
}

getIncidenceState <- function (input) {
  if (input$incidenceDataType == 'own') {
    # State 2.1
    return(2.1)
  } else if (input$incidenceDataType == 'preloaded') {
    # State 2.2
    return(2.2)
  } else {
    stop('Error: Invalid State (6). This should never happen, something went wrong.')
  }
}
