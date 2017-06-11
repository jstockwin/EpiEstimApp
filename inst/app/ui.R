#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(bootstrapPage(theme = shinytheme("spacelab"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href="styles.css"),
    tags$link(rel = "shortcut icon", type = "image/x-icon", href="favicon.ico")
  ),
  # include the js code
  useShinyjs(),
  includeScript("scripts.js"),
  column(12,
         HTML(
           "
          <h1>Transmissibility Estimator</h1>
          <p>This application estimates disease transmissibility from incidence time series and
 time-censored serial interval data.</p>
          <p>The authors request users to cite the original publication (TODO: LINK) when referring to this
tool, the format or results generated from it.</p>
          <p>TODO: Insert citatation.</p>
          <p>For information on the preloaded datasets, <a href='https://github.com/jstockwin/EpiEstimApp/wiki/Preloaded-Datasets'>click here</a>.</p>
          <p>For more information on uploading your own data, <a href='https://github.com/jstockwin/EpiEstimApp/wiki/Uploading-your-own-data'>click here</a>.</p>
          "
         )      
  ),
  column(4, id="menu",
         div(id="status",
             verbatimTextOutput('output')
         ),
         div(id="titles",
             div(id="incidenceTitle", h1('Incidence Data')),
             hidden(div(id="SITitle", h1("Serial Interval (SI) Input")))
          ),
         div(id="1.1",
             div(id="incidenceDataTypeErrorBox", class="ErrorBox",
               radioButtons('incidenceDataType', 'Do you want to use pre-loaded incidence time series data or upload your own?',
                            c('Pre-loaded' = 'preloaded', 'Own data' = 'own'))
             )
         ),
         hidden(div(id="2.1",
                    # State 2.1
                    div(id="incidenceDataErrorBox", class="ErrorBox",
                        fileInput('incidenceData', 'Choose incidence data file to upload',
                                  accept = c(
                                    'text/csv',
                                    'text/comma-separated-values',
                                    'text/tab-separated-values',
                                    'text/plain',
                                    '.csv',
                                    '.tsv'
                                  )
                        )
                      ),
                    checkboxInput('incidenceHeader', 'Header', FALSE),
                    radioButtons('incidenceSep', 'Separator',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 ','),
                    radioButtons('incidenceQuote', 'Quote',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 ''),
                    div(id="uploadedWidthErrorBox", class="ErrorBox",
                      sliderInput('uploadedWidth', 'Choose the width of the sliding time window for R estimation', min=1, max=20, value = 7)
                    ),
                    div(id="uploadedMeanPriorErrorBox", class="ErrorBox",
                        numericInput("uploadedMeanPrior", "Choose the prior mean value for the reproduction number estimate", value=5, min=0)
                    ),
                    div(id="uploadedStdPriorErrorBox", class="ErrorBox",
                        numericInput("uploadedStdPrior", "Choose the prior standard deviation value for the reproduction number estimate", value=5, min=0)
                    )
         )),
         hidden(div(id="2.2",
                    # State 2.2
                    div(id="incidenceDatasetErrorBox", class="ErrorBox",
                      radioButtons('incidenceDataset', 'Choose your dataset',
                                   c('PennsylvaniaH1N1', 'NewYorkH1N1', 'KiribatiRotavirus', 'Flu1918',
                                   'Flu2009', 'Measles1861', 'SARS2003', 'Smallpox1972'))
                    ),
                    div(id="incidenceWidthErrorBox", class="ErrorBox",
                      sliderInput('incidenceWidth', 'Choose the width of the sliding time window for R estimation', min=1, max=20, value = 7)
                    ),
                    div(id="incidenceMeanPriorErrorBox", class="ErrorBox",
                        numericInput("incidenceMeanPrior", "Choose the prior for the mean", value=5, min=0)
                    ),
                    div(id="incidenceStdPriorErrorBox", class="ErrorBox",
                        numericInput("incidenceStdPrior", "Choose the prior for the standard deviation", value=5, min=0)
                    )
         )),
         hidden(div(id="3.1",
                    # State 3.1
                    div(id="importedErrorBox", class="ErrorBox",
                      radioButtons('imported', "Do you have data about which cases are imported?",
                                   c('No' = 'FALSE', 'Yes'='TRUE'))
                    )
         )),
         hidden(div(id="4.1",
                    # State 4.1
                    div(id="importedDataErrorBox", class="ErrorBox",
                      fileInput('importedData', 'Choose a data file with numbers of imported cases to upload',
                                accept = c(
                                  'text/csv',
                                  'text/comma-separated-values',
                                  'text/tab-separated-values',
                                  'text/plain',
                                  '.csv',
                                  '.tsv'
                                )
                      )
                    ),
                    checkboxInput('importedHeader', 'Header', FALSE),
                    radioButtons('importedSep', 'Separator',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 ','),
                    radioButtons('importedQuote', 'Quote',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 '')
         )),
         hidden(div(id="5.1",
                    # State 5.1
                    div(id="SIPatientDataErrorBox", class="ErrorBox",
                      radioButtons('SIPatientData', paste("Do you want to estimate the serial interval from data",
                                                          "on the possible times of symptom appearance in donor-recipient pairs?"),
                                   c('No'='FALSE', 'Yes'='TRUE'))
                    )
         )),
         hidden(div(id="6.1",
                    # State 6.1
                    div(id="SIDataTypeErrorBox", class="ErrorBox",
                      radioButtons('SIDataType', 'Would you like to use an external file containing the exposure data, or a pre-loaded dataset?',
                                   c('Pre-loaded' = 'preloaded', 'Own data' = 'own'))
                    )
         )),
         hidden(div(id="6.2", 
                    # State 6.2
                    div(id="uncertaintyErrorBox", class="ErrorBox",
                      radioButtons('uncertainty', 'Would you like to allow uncertainty in the serial interval distribution?',
                                   c('No'='FALSE', 'Yes'='TRUE'))
                    )
         )),
         hidden(div(id="7.1",
                    # State 7.1
                    div(id="SIDatasetErrorBox", class="ErrorBox",
                      radioButtons('SIDataset', 'Choose your dataset',
                                   c('EcuadorRotavirus', 'NewYorkH1N1', 'USAH1N1'))
                    )
         )),
         hidden(div(id="7.2",
                    # State 7.2
                    div(id="SIFromErrorBox", class="ErrorBox",
                      radioButtons('SIFrom', 'Do you have raw exposure data or a SI posterior sample to upload?',
                                   c('Raw exposure data'='data', 'SI posterior sample'='sample'))
                    )
         )),
         hidden(div(id="7.3",
                    # State 7.3
                    div(id="n1ErrorBox", class="ErrorBox",
                      numericInput('n1', 'n1', min=2, value=50),
                      p('Positive integer describing the number of (Mean SI, Std SI) pairs to be drawn')
                    ),
                    div(id="n2ErrorBox", class="ErrorBox",
                      numericInput('n2', 'n2', min=2, value=50),
                      p('Positive integer describing the number of samples drawn from each (Mean SI, Std SI) pair')
                    ),
                    tags$hr(),
                    tags$p("Please choose values describing the serial interval distribution, and the uncertainty around these values"),
                    div(id="Mean.SIErrorBox", class="ErrorBox",
                      numericInput('Mean.SI', 'Mean.SI', value=2, min=1)
                    ),
                    div(id="Std.Mean.SIErrorBox", class="ErrorBox",
                     numericInput('Std.Mean.SI', 'Std.Mean.SI', min=0, value=1)
                    ),
                    div(id="Min.Mean.SIErrorBox", class="ErrorBox",
                      numericInput('Min.Mean.SI', 'Min.Mean.SI', min=1, value=1)
                    ),
                    div(id="Max.Mean.SIErrorBox", class="ErrorBox",
                      numericInput('Max.Mean.SI', 'Max.Mean.SI', value=3, min=1)
                    ),
                    div(id="Std.SIErrorBox", class="ErrorBox",
                      numericInput('Std.SI', 'Std.SI', value=2, min=1)
                    ),
                    div(id="Std.Std.SIErrorBox", class="ErrorBox",
                      numericInput('Std.Std.SI', 'Std.Std.SI', min=0, value=1)
                    ),
                    div(id="Min.Std.SIErrorBox", class="ErrorBox",
                      numericInput('Min.Std.SI', 'Min.Std.SI', value=1, min=1)
                    ),
                    div(id="Max.Std.SIErrorBox", class="ErrorBox",
                      numericInput('Max.Std.SI', 'Max.Std.SI', value=3, min=1)
                    ),
                    div(id="uncertainSeedErrorBox", class="ErrorBox",
                        numericInput("uncertainSeed", paste("Set a seed to be used by EpiEstim, so that the results are reproducible.",
                                     "A random seed will be chosen if this is left blank"), value=NULL)
                    )
         )),
         hidden(div(id="7.4",
                    # State 7.4
                    div(id="parametricErrorBox", class="ErrorBox",
                      radioButtons('parametric', 'Parametric on Non-Parametric SI?',
                                   c('Parametric'='TRUE', 'Non-Parametric'='FALSE'))
                    )
         )),
         hidden(div(id="8.1",
                    # State 8.1
                    div(id="SIDistErrorBox", class="ErrorBox",
                        radioButtons('SIDist', 'Choose your serial interval distribution',
                                     c('Gamma'='G',
                                       'Offset Gamma' = 'off1G',
                                       'Weibull' = 'W',
                                       'Offset Weibull' = 'off1W',
                                       'Log-Normal' = 'L',
                                       'Offset Log-Normal' = 'off1L'))
                        )
      
         )),
         hidden(div(id="8.2",
                    # State 8.2
                    div(id="SIDataErrorBox", class="ErrorBox",
                      fileInput('SIData', 'Choose serialIntervalData file to upload',
                                accept = c(
                                  'text/csv',
                                  'text/comma-separated-values',
                                  'text/tab-separated-values',
                                  'text/plain',
                                  '.csv',
                                  '.tsv'
                                )
                      )
                    ),
                    checkboxInput('SIHeader', 'Header', FALSE),
                    radioButtons('SISep', 'Separator',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 ','),
                    radioButtons('SIQuote', 'Quote',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 ''),
                    div(id="uploadedSISeedErrorBox", class="ErrorBox",
                        numericInput("uploadedSISeed", "Set a seed to be used by EpiEstim. A random one will be chosen if this is left blank", value=NULL)
                    )
         )),
         hidden(div(id="8.3",
                    # State 8.3
                    div(id="SISampleDataErrorBox", class="ErrorBox",
                      fileInput('SISampleData', 'Choose serialIntervalData file to upload',
                                accept = c(
                                  'text/csv',
                                  'text/comma-separated-values',
                                  'text/tab-separated-values',
                                  'text/plain',
                                  '.csv',
                                  '.tsv'
                                )
                      )
                    ),
                    checkboxInput('SISampleHeader', 'Header', FALSE),
                    radioButtons('SISampleSep', 'Separator',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 ','),
                    radioButtons('SISampleQuote', 'Quote',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 ''),
                    div(id="n23ErrorBox", class="ErrorBox",
                      numericInput('n23', 'Choose n2, the posterior sample size to be drawn for R for each SI distribution sampled', min=10, value=100)
                    ),
                    div(id="SISampleSeedErrorBox", class="ErrorBox",
                        numericInput("SISampleSeed", "Set a seed to be used by EpiEstim. A random one will be chosen this is left blank", value=NULL)
                    )
         )),
         hidden(div(id="8.4",
                    # State 8.4
                    div(id="Mean.SI2ErrorBox", class="ErrorBox",
                      numericInput('Mean.SI2', 'Mean.SI', value=2, min=1+1e-18)
                    ),
                    div(id="Std.SI2ErrorBox", class="ErrorBox",
                      numericInput('Std.SI2', 'Std.SI', value=1, min=1e-18)
                    )
         )),
         hidden(div(id="8.5",
                    # State 8.5
                    div(id="SIDistrDataTypeErrorBox", class="ErrorBox",
                      radioButtons('SIDistrDataType', 'Would you like to use an external file containing the SI distribution, or a pre-loaded SI distribution?',
                                   c('Pre-loaded' = 'preloaded', 'External file' = 'own'))
                    )
                    
         )),
         hidden(div(id="9.1",
                    # State 9.1
                    div(id="SIDist2ErrorBox", class="ErrorBox",
                      radioButtons('SIDist2', 'Choose your serial interval distribution',
                                   c('Gamma'='G',
                                     'Offset Gamma' = 'off1G',
                                     'Weibull' = 'W',
                                     'Offset Weibull' = 'off1W',
                                     'Log-Normal' = 'L',
                                     'Offset Log-Normal' = 'off1L'))
                    ),
                    p('NOTE: MCMC will run burnin + n1*thin iterations. This is slow. Try to keep below 10,000 even for small dataset'),
                    div(id="n12ErrorBox", class="ErrorBox",
                      numericInput('n12', 'Choose the posterior sample size (n1)', min=10, value=500)
                    ),
                    div(id="burninErrorBox", class="ErrorBox",
                      numericInput('burnin', 'Choose the number of iterations used as MCMC burnin', min=0, value=3000)
                    ),
                    div(id="thinErrorBox", class="ErrorBox",
                      numericInput('thin', 'Choose MCMC thin parameter (thin-1 out of thin iterations will be discarded to produce posterior sample)', min=1, value=10)
                    ),
                    div(id="n22ErrorBox", class="ErrorBox",
                      numericInput('n22', 'Choose n2, the posterior sample size to be drawn for R for each SI distribution sampled', min=10, value=100)
                    ),
                    div(id="MCMCSeedErrorBox", class="ErrorBox",
                        numericInput("MCMCSeed", "Set a seed to be used for MCMC. A random one will be chosen if this is left blank", value=NULL)
                    ),
                    HTML('
                                 <p>If you would like to use your own initialisation parameters for MCMC <a onClick="showMCMCParams()">click here</a>. Otherwise, a smart choice will be made by EpiEstim, which should work in most cases.</p>
                            '),
                    div(id='MCMCInitialParams', style="display: none",
                        p('For Gamma, Offset Gamma and Weibull distributions, 
                                      param1 is the shape and param2 is the scale of the distribution. 
                                      For Log-Normal, param1 and param2 are respectively 
                                      the mean and standard deviation of the logarithm.'),
                        div(id="param1ErrorBox", class="ErrorBox",
                          numericInput('param1', 'Choose the value of param1', min=0, value='')
                        ),
                        div(id="param2ErrorBox", class="ErrorBox",
                          numericInput('param2', 'Choose the value of param2', min=0, value='')
                        )
                    )
         )),
         hidden(div(id="9.2",
                    # State 9.2    
                    div(id="SIDistrDataErrorBox", class="ErrorBox",
                      fileInput('SIDistrData', 'Choose serialIntervalData file to upload',
                                accept = c(
                                  'text/csv',
                                  'text/comma-separated-values',
                                  'text/tab-separated-values',
                                  'text/plain',
                                  '.csv',
                                  '.tsv'
                                )
                      )
                    ),
                    checkboxInput('SIDistrHeader', 'Header', FALSE),
                    radioButtons('SIDistrSep', 'Separator',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 ','),
                    radioButtons('SIDistrQuote', 'Quote',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 '')
         )),
         hidden(div(id="9.3",
                    # State 9.3
                    div(id="SIDistrDatasetErrorBox", class="ErrorBox",
                      radioButtons('SIDistrDataset', 'Choose your SI.Distr Dataset',
                                   c('Flu1918', 'Flu2009', 'Measles1861',
                                     'SARS2003', 'Smallpox1972'))
                    )
         )),
         div(id="control",
             hidden(actionButton('stop', label='Stop')),
             disabled(actionButton('prev', label='Previous')),
             disabled(actionButton('nxt', label='Next')),
             hidden(actionButton('go', label='Go')),
             textOutput('error')
         )
         
  ),
  column(8, id="plot",
         tabsetPanel(
           tabPanel("Plot", downloadButton("savePlot", "Save Image"), plotOutput("plot")),
           tabPanel("Incidence Data", downloadButton("saveIncidence", "Save Table"), tableOutput("incidenceDataOutput")),
           tabPanel("Estimated Reproduction Number", downloadButton("saveR", "Save Table"), tableOutput("estimatedROutput")),
           tabPanel("Serial Interval Distribution", downloadButton("saveSI", "Save Table"), tableOutput("serialIntervalOutput"))
         )
  )
  
)
)
