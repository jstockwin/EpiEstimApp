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
          <meta name='keywords' content='infectious,disease,epidemiology,transmissibility,serial,interval,generation,time,time,varying,reproduction,number,Robin,Thompson,Anne,Cori,Jake,Stockwin,Hackout3'>
          <meta name='author' content='Jake Stockwin'>
          <h1>EpiEstim App</h1>
          <p>This web application generates an estimate of infectious disease transmissibility throughout an outbreak. The time-dependent reproduction number (<i>R</i>) is inferred from disease incidence time series and raw data or estimates of the serial interval. For more information about how to use this application, please see the <a href='https://github.com/jstockwin/EpiEstimApp/wiki'>wiki</a>. For the most part, we assume that users will want to upload their own data, however there are also some <a href='https://github.com/jstockwin/EpiEstimApp/wiki/Preloaded-Datasets' target='_blank'>example datasets</a> built in, to illustrate how the application might be used.</p>
          <p>For more information on uploading your own data and on the required format of the data, please
          <a href='https://github.com/jstockwin/EpiEstimApp/wiki/Uploading-your-own-data' target='_blank'>click here</a>.</p>
          <p>The authors request users to cite the original publication when referring to this tool, any results generated from it, or the R software application on which this tool is based (EpiEstim 2):

</p>
          <p>Thompson RN, Stockwin JE, van Gaalen RD, Polonsky JA, et al. EpiEstim 2: An improved tool for estimating serial intervals and time-varying reproduction numbers during infection disease outbreaks. Submitted (2017).</p>
          "
         ) 
  ),
  column(4, id="menu",
         div(id="status",
             verbatimTextOutput('output'),
             htmlOutput("progress")
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
                    h2("R Estimation Settings"),
                    div(id="incidenceDataErrorBox", class="ErrorBox",
                        fileInput('incidenceData', 'Choose incidence data file to upload',
                                  accept = c(
                                    'text/csv',
                                    'text/comma-separated-values',
                                    'text/plain',
                                    '.csv'
                                  )
                        )
                      ),
                    checkboxInput('incidenceHeader', 'Header', FALSE),
                    div(id="uploadedWidthErrorBox", class="ErrorBox",
                      sliderInput('uploadedWidth', 'Choose the width of the sliding time window for R estimation', min=1, max=20, value = 7)
                    ),
                    div(id="uploadedMeanPriorErrorBox", class="ErrorBox",
                        numericInput("uploadedMeanPrior", "Choose the prior mean value for R", value=5, min=0)
                    ),
                    div(id="uploadedStdPriorErrorBox", class="ErrorBox",
                        numericInput("uploadedStdPrior", "Choose the prior standard deviation value for R", value=5, min=0)
                    )
         )),
         hidden(div(id="2.2",
                    # State 2.2
                    h2("R Estimation Settings"),
                    div(id="incidenceDatasetErrorBox", class="ErrorBox",
                      radioButtons('incidenceDataset', 'Choose your dataset',
                                   c('H1N1Pennsylvania2009', 'H1N1NewYork2009', 'RotavirusKiribati2013', 'H1N1Maryland1918',
                                   'MeaslesGermany1861', 'SARSHongKong2003', 'SmallpoxKosovo1972'))
                    ),
                    div(id="incidenceWidthErrorBox", class="ErrorBox",
                      sliderInput('incidenceWidth', 'Choose the width of the sliding time window for R estimation', min=1, max=20, value = 7)
                    ),
                    div(id="incidenceMeanPriorErrorBox", class="ErrorBox",
                        numericInput("incidenceMeanPrior", "Choose the prior mean value for R", value=5, min=0)
                    ),
                    div(id="incidenceStdPriorErrorBox", class="ErrorBox",
                        numericInput("incidenceStdPrior", "Choose the prior standard deviation value for R", value=5, min=0)
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
                                  'text/plain',
                                  '.csv'
                                )
                      )
                    ),
                    checkboxInput('importedHeader', 'Header', FALSE)
         )),
         hidden(div(id="5.1",
                    # State 5.1
                    div(id="SIPatientDataErrorBox", class="ErrorBox",
                      radioButtons('SIPatientData', paste("Do you want to use serial interval data from individual patients, or use",
                                                          "a distributional estimate of the serial interval?"),
                                   c('Distributional Estimate'='FALSE', 'Individual Patient Data'='TRUE'))
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
                    div(id="SIEstTypeErrorBox", class="ErrorBox",
                      radioButtons('SIEstType', 'Which of the following serial interval distribution estimates would you like to use?',
                                   c(
                                      "Parametric with uncertainty (offset gamma)"="uncertain",
                                      "Parametric without uncertainty (offset gamma)"="parametric",
                                      "Upload your own probability distribution"="own",
                                      "Use a distribution estimated from a previous outbreak (preloaded data)"="preloaded"
                                    ))
                    )
         )),
         hidden(div(id="7.1",
                    # State 7.1
                    p("Please note that these preloaded data are only to give an idea as to how the app works. As such, the full range of options are not provided here (e.g. the number of steps in the MCMC chain is fixed)."),
                    HTML("<p>If you would like to have the full range of options, please download the serial interval data for your dataset from <a href='https://github.com/jstockwin/EpiEstimApp/tree/master/inst/app/datasets/SerialIntervalData' target='_blank'>here</a> then go back a step and select 'Own data' then 'Raw exposure data'.</p>"),
                    div(id="SIDatasetErrorBox", class="ErrorBox",
                      radioButtons('SIDataset', 'Choose your dataset',
                                   c('RotavirusEcuador2011', 'H1N1NewYork2009', 'H1N1USA2009'))
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
                      p('Choose the number of serial interval distributions to be sampled (n1) according to the parameters below')
                    ),
                    div(id="n2ErrorBox", class="ErrorBox",
                      numericInput('n2', 'n2', min=2, value=50),
                      p('Choose the number of serial interval valies to be samples from each of the n1 estimated serial interval distributions in order to estimate R (n2)')
                    ),
                    tags$hr(),
                    tags$p("Choose values describing the serial interval distribution, and the uncertainty around these values"),
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
                    div(id="Mean.SI2ErrorBox", class="ErrorBox",
                      numericInput('Mean.SI2', 'Mean.SI', value=2, min=1+1e-18)
                    ),
                    div(id="Std.SI2ErrorBox", class="ErrorBox",
                      numericInput('Std.SI2', 'Std.SI', value=1, min=1e-18)
                    )
         )),
         hidden(div(id="7.5",
                    # State 7.5
                    div(id="SIDistrDataErrorBox", class="ErrorBox",
                      fileInput('SIDistrData', 'Choose serialIntervalData file to upload',
                                accept = c(
                                  'text/csv',
                                  'text/comma-separated-values',
                                  'text/plain',
                                  '.csv'
                                )
                      )
                    ),
                    checkboxInput('SIDistrHeader', 'Header', FALSE)
         )),
         hidden(div(id="7.6",
                    # State 7.6
                    div(id="SIDistrDatasetErrorBox", class="ErrorBox",
                      radioButtons('SIDistrDataset', 'Choose your SI.Distr Dataset',
                                   c('H1N1Maryland1918', 'H1N1Pennsylvania2009', 'MeaslesGermany1861',
                                     'SARSHongKong2003', 'SmallpoxKosovo1972'))
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
                        ),
                    div(id="n24ErrorBox", class="ErrorBox",
                        numericInput('n24', 'Choose n2, the posterior sample size to be drawn for R for each SI distribution sampled', min=10, value=100)
                        ),
                    div(id="preloadedSeedErrorBox", class="ErrorBox",
                        numericInput("preloadedSeed", "Set a seed to be used by EpiEstim. A random one will be chosen if this is left blank", value=NULL)
                        )
         )),
         hidden(div(id="8.2",
                    # State 8.2
                    div(id="SIDataErrorBox", class="ErrorBox",
                      fileInput('SIData', 'Choose serialIntervalData file to upload',
                                accept = c(
                                  'text/csv',
                                  'text/comma-separated-values',
                                  'text/plain',
                                  '.csv'
                                )
                      )
                    ),
                    checkboxInput('SIHeader', 'Header', FALSE),
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
                                  'text/plain',
                                  '.csv'
                                )
                      )
                    ),
                    checkboxInput('SISampleHeader', 'Header', FALSE),
                    div(id="n23ErrorBox", class="ErrorBox",
                      numericInput('n23', 'Choose n2, the posterior sample size to be drawn for R for each SI distribution sampled', min=10, value=100)
                    ),
                    div(id="SISampleSeedErrorBox", class="ErrorBox",
                        numericInput("SISampleSeed", "Set a seed to be used by EpiEstim. A random one will be chosen if this is left blank", value=NULL)
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
                    p('NOTE: MCMC will run burnin + n1*thin iterations. This is slow. Try to keep below 10,000 even for small datasets. For longer MCMC chains, please use the EpiEstim 2.0 R package directly, rather than our online interface.'),
                    div(id="n12ErrorBox", class="ErrorBox",
                      numericInput('n12', 'Choose the number of serial interval distributions to be estimated using MCMC (n1)', min=10, value=500)
                    ),
                    div(id="burninErrorBox", class="ErrorBox",
                      numericInput('burnin', 'Choose the number of iterations used as MCMC burn in', min=0, value=3000)
                    ),
                    div(id="thinErrorBox", class="ErrorBox",
                      numericInput('thin', 'Choose the MCMC thinning parameter (thin). Each of the n1 estimated serial interval distributions will be taken after every "thin" iterations of the MCMC chain.', min=1, value=10)
                    ),
                    div(id="n22ErrorBox", class="ErrorBox",
                      numericInput('n22', 'Choose the number of serial interval values to be sampled from each estimated serial interval distribution in order to estimate R (n2)', min=10, value=100)
                    ),
                    div(id="MCMCSeedErrorBox", class="ErrorBox",
                        numericInput("MCMCSeed", "Set a seed for the MCMC. If no seed is chosen, a random value will be selected.", value=NULL)
                    ),
                    tags$hr(),
                    p('The following two parameters give the initial parameters to use in MCMC.
                      For Gamma, Offset Gamma and Weibull distributions,
                                  param1 is the shape and param2 is the scale of the distribution.
                                  For Log-Normal, param1 and param2 are respectively
                                  the mean and standard deviation of the logarithm.'),
                    div(id="param1ErrorBox", class="ErrorBox",
                      numericInput('param1', 'Choose the value of param1', min=0, value='')
                    ),
                    div(id="param2ErrorBox", class="ErrorBox",
                      numericInput('param2', 'Choose the value of param2', min=0, value='')
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
