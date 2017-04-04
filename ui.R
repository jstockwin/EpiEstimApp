#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(bootstrapPage(
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
          "
         )      
  ),
  column(4, id="menu",
         div(id="status",
             verbatimTextOutput('output')
         ),
         div(id="1.1",
             h1('Incidence Data'),
             radioButtons('incidenceDataType', 'Do you want to use pre-loaded incidence time series data or upload your own?',
                          c('Pre-loaded' = 'preloaded', 'Own data' = 'own'))
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
                    sliderInput('uploadedWidth', 'Choose the width of the sliding time window for R estimation', min=1, max=20, value = 7)
         )),
         hidden(div(id="2.2",
                    # State 2.2
                    radioButtons('incidenceDataset', 'Choose your dataset',
                                 c('PennsylvaniaH1N12009', 'RotavirusGermany',
                                   'Flu1918', 'Flu2009', 'Measles1861', 'SARS2003', 'Smallpox1972')),
                    sliderInput('incidenceWidth', 'Choose the width of the sliding time window for R estimation', min=1, max=20, value = 7)
         )),
         hidden(div(id="3.1", 
                    # State 3.1
                    radioButtons('imported', "Do you have data about which cases are imported?",
                                 c('No' = 'FALSE', 'Yes'='TRUE')),
                    conditionalPanel('input.imported == "FALSE"', 
                                     # If no, "Next" should continue to state 5.1
                                     div(class='continue')
                    )
         )),
         hidden(div(id="4.1",
                    # State 4.1
                    fileInput('importedData', 'Choose an imported data file to upload',
                              accept = c(
                                'text/csv',
                                'text/comma-separated-values',
                                'text/tab-separated-values',
                                'text/plain',
                                '.csv',
                                '.tsv'
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
                                 ''),
                    div(class="continue")
         )),
         hidden(div(id="5.1",
                    # State 5.1
                    radioButtons('SIPatientData', 'Do you want to use exposure data to inform the SI?',
                                 c('No'='FALSE', 'Yes'='TRUE'))
         )),
         hidden(div(id="6.1",
                    # State 6.1
                    radioButtons('SIDataType', 'Would you like to use an external file containing the exposure data, or a pre-loaded dataset?',
                                 c('Pre-loaded' = 'preloaded', 'Own data' = 'own'))
         )),
         hidden(div(id="6.2", 
                    # State 6.2
                    radioButtons('uncertainty', 'Would you like to include SI uncertainty in your model?',
                                 c('No'='FALSE', 'Yes'='TRUE'))
         )),
         hidden(div(id="7.1",
                    # State 7.1
                    radioButtons('SIDataset', 'Choose your dataset',
                                 c('PennsylvaniaH1N12009', 'RotavirusGermany'))
         )),
         hidden(div(id="7.2",
                    # State 7.2
                    radioButtons('SIFrom', 'Do you have raw exposure data or a SI posterior sample to upload?',
                                 c('Raw exposure data'='data', 'SI posterior sample'='sample'))
         )),
         hidden(div(id="7.3",
                    # State 7.3
                    
                    numericInput('n1', 'n1', min=2, value=50),
                    p('positive integer giving the size of the sample of pairs (Mean SI (serial interval), Std SI) to be drawn'),
                    numericInput('n2', 'n2', min=2, value=50),
                    p('positive integer giving the size of the sample drawn from each posterior distribution conditional to a pair (Mean SI, Std SI)'),
                    numericInput('Mean.SI', 'Mean.SI', value=2, min=1),
                    numericInput('Std.Mean.SI', 'Std.Mean.SI', min=0, value=1),
                    numericInput('Min.Mean.SI', 'Min.Mean.SI', min=1, value=1),
                    numericInput('Max.Mean.SI', 'Max.Mean.SI', value=3, min=1),
                    numericInput('Std.SI', 'Std.SI', value=2, min=1),
                    numericInput('Std.Std.SI', 'Std.Std.SI', min=0, value=1),
                    numericInput('Min.Std.SI', 'Min.Std.SI', value=1, min=1),
                    numericInput('Max.Std.SI', 'Max.Std.SI', value=3, min=1)
         )),
         hidden(div(id="7.4",
                    # State 7.4
                    radioButtons('parametric', 'Parametric on Non-Parametric SI?',
                                 c('Parametric'='TRUE', 'Non-Parametric'='FALSE'))
         )),
         hidden(div(id="8.1",
                    # State 8.1
                    div(id="SIDistErrorBox", class="ErrorBox",
                        radioButtons('SIDist', 'Choose your serial interval distribution',
                                     c('Gamma'='G',
                                       'Offset Gamma'='off1G',
                                       'Erlang' = 'E',
                                       'Weibull' = 'W',
                                       'Log-Normal' = 'L'))
                        )
      
         )),
         hidden(div(id="8.2",
                    # State 8.2
                    
                    fileInput('SIData', 'Choose serialIntervalData file to upload',
                              accept = c(
                                'text/csv',
                                'text/comma-separated-values',
                                'text/tab-separated-values',
                                'text/plain',
                                '.csv',
                                '.tsv'
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
                                 '')
         )),
         hidden(div(id="8.3",
                    # State 8.3
                    
                    fileInput('SISampleData', 'Choose serialIntervalData file to upload',
                              accept = c(
                                'text/csv',
                                'text/comma-separated-values',
                                'text/tab-separated-values',
                                'text/plain',
                                '.csv',
                                '.tsv'
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
                    numericInput('n23', 'Choose n2, the posterior sample size to be drawn for R for each SI distribution sampled', min=10, value=100)
         )),
         hidden(div(id="8.4",
                    # State 8.4
                    
                    numericInput('Mean.SI2', 'Mean.SI', value=2, min=1+1e-18),
                    numericInput('Std.SI2', 'Std.SI', value=1, min=1e-18)
         )),
         hidden(div(id="8.5",
                    # State 8.5
                    radioButtons('SIDistrDataType', 'Would you like to use an external file containing the SI distribution, or a pre-loaded SI distribution?',
                                 c('Pre-loaded' = 'preloaded', 'External file' = 'own'))
                    
         )),
         hidden(div(id="9.1",
                    # State 9.1
                    
                    radioButtons('SIDist2', 'Choose your serial interval distribution',
                                 c('Gamma'='G',
                                   'Offset Gamma'='off1G',
                                   'Erlang' = 'E',
                                   'Weibull' = 'W',
                                   'Log-Normal' = 'L')),
                    p('NOTE: MCMC will run burnin + n1*thin iterations. This is slow. Try to keep below 10,000 even for small dataset'),
                    numericInput('n12', 'Choose the posterior sample size (n1)', min=10, value=500),
                    numericInput('burnin', 'Choose the number of iterations used as MCMC burnin', min=0, value=3000),
                    numericInput('thin', 'Choose MCMC thin parameter (thin-1 out of thin iterations will be discarded to produce posterior sample)', min=1, value=10),
                    numericInput('n22', 'Choose n2, the posterior sample size to be drawn for R for each SI distribution sampled', min=10, value=100),
                    tags$hr(),
                    HTML('
                                 <p>If you would like to use your own initialisation parameters for MCMC <a onClick="showMCMCParams()">click here</a>. Otherwise, a smart choice will be made by EpiEstim, which should work in most cases.</p>
                            '),
                    div(id='MCMCInitialParams', style="display: none",
                        p('For Gamma, Offset Gamma, Erlang and Weibull distributions, 
                                      param1 is the shape and param2 is the scale of the distribution. 
                                      For Log-Normal, param1 and param2 are respectively 
                                      the mean and standard deviation of the logarithm.'),
                        numericInput('param1', 'Choose the value of param1', min=0, value=''),
                        numericInput('param2', 'Choose the value of param2', min=0, value='')
                    )
                    
         )),
         hidden(div(id="9.2",
                    # State 9.2    
                    
                    fileInput('SIDistrData', 'Choose serialIntervalData file to upload',
                              accept = c(
                                'text/csv',
                                'text/comma-separated-values',
                                'text/tab-separated-values',
                                'text/plain',
                                '.csv',
                                '.tsv'
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
                    
                    radioButtons('SIDistrDataset', 'Choose your SI.Distr Dataset',
                                 c('Flu1918', 'Flu2009', 'Measles1861',
                                   'SARS2003', 'Smallpox1972'))
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
         plotOutput('plot')
  )
  
)
)
