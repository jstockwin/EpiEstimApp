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



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Calculating fit takes a long time. We'll make it reactive
  # so that it only updated when a new serialIntervalDataFile is supplied.
  
  get_uploaded_fit <- reactive({

    serialIntervalData <- read.csv(input$serialIntervalData$datapath, 
                                   header = input$header, sep = input$sep,
                                   quote = input$quote)
    
    # Process the data (see function in utils.R)
    serialIntervalData <- processSerialIntervalData(serialIntervalData)
    
    # Only use 80 host pairs' interval data to estimate the serial interval
    return(dic.fit.mcmc(dat = serialIntervalData, dist=input$SIDist))
  })
   
  output$plot <- renderPlot({
    withProgress(message = 'Processing...', value=0, {
      
      
      if (input$data == 'PennsylvaniaH1N12009') {
        casesPerDayData <- read.table('datasets/PennsylvaniaH1N12009FluData.csv',
                                      header = F, sep=',')
        load('datasets/PennsylvaniaH1N12009_fit.RData')
        fit <- get(paste('pennsylvaniaH1N12009_fit', input$SIDist, sep='_'))
      } else if (input$data == 'RotavirusGermany') {
        casesPerDayData <- read.table('datasets/GermanyRotavirus1516.csv',
                                      header = F, sep=',')
        load('datasets/Rotavirus_fit.RData')
        fit <- get(paste('rotavirus_fit', input$SIDist, sep='_'))
      } else if (input$data == 'Uploaded Data'){
        serialIntervalDataFile <- input$serialIntervalData
        casesPerDayDataFile <- input$casesPerDayData
        
        if (is.null(serialIntervalDataFile)) {
          return(NULL)
        }
        
        incProgress(1/10, detail = paste("Processing interval data, this may take several minutes..."))
        
        fit = get_uploaded_fit()
        
        if (is.null(casesPerDayDataFile)) {
          return(NULL)
        }
        
        casesPerDayData <- read.csv(casesPerDayDataFile$datapath, 
                                    header = input$header, sep = input$sep,
                                    quote = input$quote)
      } else {
        return(NULL)
      }
      
      
      incProgress(1/2, detail = paste("Processing day data"))
      
      # Process data (see utils.R)
      casesPerDayData <- processCasesPerDayData(casesPerDayData)
      
      ####  FEED INTO EPIESTIM
      W <- input$W
      length <- dim(casesPerDayData)[1]
      incProgress(3/4, detail = paste("Plotting graphs..."))
      EstimateR(casesPerDayData[,2], T.Start=1:(length - W), T.End=(1+W):length, n2 = dim(fit@samples)[2], CDT = fit, plot=TRUE)
      incProgress(1/10, detail = paste("Done"))
    })
  })
  
})
