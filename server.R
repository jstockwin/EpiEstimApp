library(devtools)
install_github("nickreich/coarseDataTools", ref = "hackout3")
library(coarseDataTools)
library(MCMCpack)
library(EpiEstim)
library(shiny)

# Source necessary files
source("src/dic.fit.R", local=TRUE)
source("src/dic.fit.mcmc.R", local=TRUE)
source("src/EstimateR.R", local=TRUE)
source("src/coarse2estim.R", local=TRUE)
source("src/EstimateRnew.R", local=TRUE)
source("src/DiscrSI.R", local=TRUE)
source("src/OverallInfectivity.R", local=TRUE)
source("src/stochasticSEIRModel3.R", local=TRUE)
source("utils.R", local=TRUE)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)
options(shiny.reactlog=TRUE) 



shinyServer(function(input, output) {
   
  output$plot <- renderPlot({
    # Start a progress bar
    withProgress(message = 'Processing...', value=0, {
      incProgress(1/100, detail = paste("Initialising..."))
      # This if-else handles the special case of uploaded data slightly differently.
      if (input$data == 'Uploaded Data'){
        # Read the data
        serialIntervalDataFile <- input$serialIntervalData
        casesPerDayDataFile <- input$casesPerDayData
        
        if (is.null(serialIntervalDataFile)) {
          return(NULL)
        }
        
        incProgress(1/10, detail = paste("Processing interval data, this may take several minutes..."))
        
        # Runs the reactive function defined below which will take a long time if the uploaded data
        # is new or has changed, but will otherwise immediately complete allowing the thread to move on quickly
        # if the data has not changed (helpful when e.g. changing the width, W, only).
        fit = get_uploaded_fit()
        
        if (is.null(casesPerDayDataFile)) {
          return(NULL)
        }
        
        incProgress(1/2, detail = paste("Processing casesPerDayData"))
        
        # Load the casesPerDay data
        casesPerDayData <- read.csv(casesPerDayDataFile$datapath, 
                                    header = input$header, sep = input$sep,
                                    quote = input$quote)
        # Process casesPerDay data (see utils.R)
        casesPerDayData <- processCasesPerDayData(casesPerDayData)
      } else {
        # Load the data
        incProgress(1/2, detail = paste("Loading the data"))
        casesPerDayData <- getCasesPerDayData(input$data)
        serialIntervalData <- getSerialIntervalData(input$data)
        
        # If the distribution is set to offset gamma, we should check this is reasonable.
        if (input$SIDist == 'off1G' && any(serialIntervalData[4] - serialIntervalData[1] < 1)) {
          stop('The chosen dataset has serial intervals which are definitely less than 1,
             so a gamma distribution offset by 1 is not appropriate.')
        }
        # Get the MCMCFit (see utils.R)
        fit <- getMCMCFit(input$data, input$SIDist)
        
      }
      
      
      ####  FEED INTO EPIESTIM
      W <- input$W
      length <- dim(casesPerDayData)[1]
      incProgress(3/4, detail = paste("Plotting graphs..."))
      EstimateR(casesPerDayData[,2], T.Start=1:(length - W), T.End=(1+W):length, n2 = dim(fit@samples)[2], CDT = fit, plot=TRUE)
      incProgress(1, detail = paste("Done"))
      
    }) # End withProgress
  }) # End output$plot
  
  # Calculating fit takes a long time. We'll make it reactive
  # so that it only updated when a new serialIntervalDataFile is supplied.
  get_uploaded_fit <- reactive({
    
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
    return(dic.fit.mcmc(dat = serialIntervalData, dist=input$SIDist))
  }) # End get_uploaded_fit
  
}) # End shinyServer
