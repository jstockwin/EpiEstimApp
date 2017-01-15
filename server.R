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
alldatasets <- list(Measles1861,Flu1918,Smallpox1972,SARS2003,Flu2009)


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
    input$mydata
    if (is.null(input$mydata)) {
      # Tell client we're 'done' to say we're ready.
      session$sendCustomMessage(type='done', "")
      return()
      }
    # Put everything in a try catch, as we need to send the "DONE" message to the client
    # even if we exit with an error.
    tryCatch({
      if (input$mydata == "STOP") return()
      isolate({
        
        # Work out which state we're in
        SIState <- getSIState(input)
        incidenceState <- getIncidenceState(input)
        
        ####################
        ## Incidence Data ##
        ####################
        
        if (incidenceState == 2.1) {
          # Handle uploaded data:
          casesPerDayData <- read.csv(input$incidenceData$datapath, 
                                      header = input$incidenceHeader, sep = input$incidenceSep,
                                      quote = input$incidenceQuote)
          # Process casesPerDay data (see utils.R)
          casesPerDayData <- processCasesPerDayData(casesPerDayData)
        } else if (incidenceState == 2.2) {
          # Get preloaded data
          casesPerDayData <- getCasesPerDayData(input$incidenceDataset)
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
          length <- dim(casesPerDayData)[1]
          EstimateR(casesPerDayData[,2], T.Start=1:(length - W), T.End=(1+W):length, n2 = dim(MCMC@samples)[2], CDT = MCMC, plot=TRUE)
          session$sendCustomMessage(type='done', "")
          
        } else if (SIState == 6.2) {
          # "NonParametricUncertainSI"
          # Uploaded data, need to run MCMC. Run the next 80 iterations.
          MCMC = run_MCMC()
          
          if (dim(MCMC@samples)[1] < 8000) {
            # We are not done. Check if client wants to stop
            data <- toJSON(MCMC@samples)
            session$sendCustomMessage(type='pingToClient', data) 
            return()
          }
          
          # If we reach here, we're done with MCMC
          MCMC@samples <- MCMC@samples[3000:8000,] #Remove burnin
          ####  FEED INTO EPIESTIM
          W <- input$Width
          length <- dim(casesPerDayData)[1]
          EstimateR(casesPerDayData[,2], T.Start=1:(length - W), T.End=(1+W):length, n2 = dim(MCMC@samples)[2], CDT = MCMC, plot=TRUE)
          session$sendCustomMessage(type='done', "")
        } else if (SIState == 6.3) {
          # "UncertainSI"
          ####  FEED INTO EPIESTIM
          W <- input$Width
          length <- dim(casesPerDayData)[1]
          EstimateR(casesPerDayData[,2], T.Start=1:(length - W), T.End=(1+W):length, method="UncertainSI", n1=input$n1, n2=input$n2,
                    Mean.SI=input$Mean.SI, Std.SI=input$Std.SI,
                    Std.Mean.SI=input$Std.Mean.SI, Min.Mean.SI=input$Min.Mean.SI, Max.Mean.SI=input$Max.Mean.SI, 
                    Std.Std.SI=input$Std.Std.SI, Min.Std.SI=input$Min.Std.SI, Max.Std.SI=input$Max.Std.SI, plot=TRUE)
          session$sendCustomMessage(type='done', "")
        } else if (SIState == 6.4) {
          # "ParametricSI"
          ####  FEED INTO EPIESTIM
          W <- input$Width
          length <- dim(casesPerDayData)[1]
          EstimateR(casesPerDayData[,2], T.Start=1:(length - W), T.End=(1+W):length, Mean.SI=input$Mean.SI2, Std.SI=input$Std.SI2,
                    method="ParametricSI", plot=TRUE)
          session$sendCustomMessage(type='done', "")
        } else if (SIState == 6.5) {
          # "NonParametricSI"
          ####  FEED INTO EPIESTIM
          W <- input$Width
          length <- dim(casesPerDayData)[1]
          if (input$SIDistrDataset == 'Uploaded Data') {
            SI.Distr = read.csv(input$SIDistrData$datapath, 
                                header = input$SIDistrHeader, sep = input$SIDistrSep,
                                quote = input$SIDistrQuote)
          } else {
            SI.Distr = alldatasets[[as.numeric(input$SIDistrDataset)]]$SI.Distr
          }
          
          EstimateR(casesPerDayData[,2], T.Start=1:(length - W), T.End=(1+W):length, method='NonParametricSI', SI.Distr=SI.Distr, plot=TRUE)
          session$sendCustomMessage(type='done', "")
        }
      }) # End Isolate
    },
    error = function (e) {
      # Send message to client that we're done.
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
    if (input$SIDist == 'off1G' && any(serialIntervalData[4] - serialIntervalData[1] < 1)) {
      stop('The chosen dataset has serial intervals which are definitely less than 1,
             so a gamma distribution offset by 1 is not appropriate.')
    }
    
    # Only use 80 host pairs' interval data to estimate the serial interval
    if (is.na(input$param1) || is.na(input$param1)) {
      params = c(1,1)
    } else {
      params = c(input$param1, input$param2)
    }
    if (is.null(input$mydata) || input$mydata == "NEW") {
      return(dic.fit.mcmc.incremental(dat = serialIntervalData, dist=input$SIDist,
                                      init.pars = params, increment.size = 80))
    } else {
      current = as.data.frame(fromJSON(input$mydata))
      return(dic.fit.mcmc.incremental(dat = serialIntervalData, dist=input$SIDist,
                                      current.samples = current, increment.size = 80))
    }
    
    
  } # End get_uploaded_fit
  
}) # End shinyServer


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
