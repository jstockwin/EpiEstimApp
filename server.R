library(devtools)
install_github("nickreich/coarseDataTools", ref = "hackout3")
library(coarseDataTools)
library(MCMCpack)
library(EpiEstim)
library(shiny)

# Source necessary files
source("dic.fit.R", local=TRUE)
source("dic.fit.mcmc.R", local=TRUE)
source("EstimateR.R", local=TRUE)
source("coarse2estim.R", local=TRUE)
source("EstimateRnew.R", local=TRUE)
source("DiscrSI.R", local=TRUE)
source("OverallInfectivity.R", local=TRUE)
source("stochasticSEIRModel3.R", local=TRUE)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)
options(shiny.reactlog=TRUE) 



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Calculating fit takes a long time. We'll make it reactive
  # so that it only updated when a new serialIntervalDataFile is supplied.
  
  get_fit <- reactive({

    serialIntervalData <- read.csv(input$serialIntervalData$datapath, 
                                   header = input$header, sep = input$sep,
                                   quote = input$quote)
    
    num_cols = dim(serialIntervalData)[2]
    if (num_cols < 4 || num_cols > 5) {
      stop("serialIntervalData should have 4 or 5 columns")
    } else if (num_cols == 4) {
      # Add the type column manually
      serialIntervalData[5] <- 0
    }
    # serialIntervalData will now have 5 columns.
    names <- c("EL", "ER", "SL", "SR", "type")
    colnames(serialIntervalData) <- names
    serialIntervalData <- as.data.frame(serialIntervalData)
    
    # Only use 80 host pairs' interval data to estimate the serial interval
    return(dic.fit.mcmc(dat = serialIntervalData, dist="G"))
  })
   
  output$plot <- renderPlot({
    serialIntervalDataFile <- input$serialIntervalData
    casesPerDayDataFile <- input$casesPerDayData
    
    if (is.null(serialIntervalDataFile)) {
      return(NULL)
    }
    
    fit = get_fit()
    
    if (is.null(casesPerDayDataFile)) {
      return(NULL)
    }
    
    casesPerDayData <- read.csv(casesPerDayDataFile$datapath, 
                                   header = input$header, sep = input$sep,
                                quote = input$quote)
    
    ## Pre-process the casesPerDayData
    
    cases_dims <- dim(casesPerDayData)
    if ((cases_dims[1] == 1 && cases_dims[2] > 1) || (cases_dims[1] == 2 && cases_dims[2] > 2)) {
      # The data is transposed.
      casesPerDayData <- t(casesPerDayData)
      # Update cases_dims for next bit
      cases_dims <- dim(casesPerDayData)
    }
    
    if (cases_dims[2] > 2) {
      # Bad input
      stop("casesPerDayData should only have one column, or one column and an index column")
    } else if (cases_dims[2] == 1) {
      # Add a time column first.
      casesPerDayData <- cbind(seq.int(nrow(casesPerDayData)), casesPerDayData)
    }
    colnames(casesPerDayData) <- c("Time", "Cases")
    casesPerDayData <- as.data.frame(casesPerDayData)
    
    ####  FEED INTO EPIESTIM
    W <- input$W
    EstimateR(casesPerDayData[,2], T.Start=1:(cases_dims[1] - W), T.End=(1+W):cases_dims[1], n2 = dim(fit@samples)[2], CDT = fit, plot=TRUE)
  })
  
})
