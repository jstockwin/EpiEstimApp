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


# Source necessary files
source("src/dic.fit.mcmc.incremental.R", local=TRUE)
source("src/DiscrSI.R", local=TRUE)
source("src/OverallInfectivity.R", local=TRUE)
source("src/stochasticSEIRModel3.R", local=TRUE)
source("utils.R", local=TRUE)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)
options(shiny.reactlog=TRUE) 



shinyServer(function(input, output, session) {
   
  output$plot <- renderPlot({
    input$go
    input$mydata
    # Put everything in a dry catch, as we need to send the "DONE" message to the client
    # even if we exit with an error.
    tryCatch({
      if (!is.null(input$mydata) && input$mydata == "STOP") return()
      isolate({
        # This if-else handles the special case of uploaded data slightly differently.
        if (input$data == 'Uploaded Data'){
          # Read the data
          serialIntervalDataFile <- input$serialIntervalData
          casesPerDayDataFile <- input$casesPerDayData
          
          if (is.null(serialIntervalDataFile)) {
            return(NULL)
          }
          
          # Runs the reactive function defined below which will take a long time if the uploaded data
          # is new or has changed, but will otherwise immediately complete allowing the thread to move on quickly
          # if the data has not changed (helpful when e.g. changing the width, W, only).
          samples = get_uploaded_samples()
          
          if (dim(samples@samples)[1] < 8000) {
            # We are not done. Check if client wants to stop
            data <- toJSON(samples@samples)
            session$sendCustomMessage(type='pingToClient', data) 
            return()
          }
          
          samples@samples <- samples@samples[3000:8000,] #Remove burnin
          
          if (is.null(casesPerDayDataFile)) {
            return(NULL)
          }
          
          # Load the casesPerDay data
          casesPerDayData <- read.csv(casesPerDayDataFile$datapath, 
                                      header = input$header, sep = input$sep,
                                      quote = input$quote)
          # Process casesPerDay data (see utils.R)
          casesPerDayData <- processCasesPerDayData(casesPerDayData)
        } else {
          # Load the data
          casesPerDayData <- getCasesPerDayData(input$data)
          serialIntervalData <- getSerialIntervalData(input$data)
          
          # If the distribution is set to offset gamma, we should check this is reasonable.
          if (input$SIDist == 'off1G' && any(serialIntervalData[4] - serialIntervalData[1] < 1)) {
            stop('The chosen dataset has serial intervals which are definitely less than 1,
             so a gamma distribution offset by 1 is not appropriate.')
          }
          # Get the MCMCFit (see utils.R)
          samples <- getMCMCFit(input$data, input$SIDist)
          
        }
        
        
        ####  FEED INTO EPIESTIM
        W <- input$W
        length <- dim(casesPerDayData)[1]
        EstimateR(casesPerDayData[,2], T.Start=1:(length - W), T.End=(1+W):length, n2 = dim(samples@samples)[2], CDT = samples, plot=TRUE)
        session$sendCustomMessage(type='done', "")
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
  get_uploaded_samples <- function(data) {
    
    serialIntervalData <- read.csv(input$serialIntervalData$datapath, 
                                   header = input$header, sep = input$sep,
                                   quote = input$quote)
    
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
