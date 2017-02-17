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
             tags$div(id="status",
                      verbatimTextOutput('output')
                      ),
             tags$div(id="1",
                      h1('Indicence Data'),
                      radioButtons('incidenceDataType', 'Do you want to use pre-loaded incidence time series data or upload your own?',
                                   c('Pre-loaded' = 'preloaded', 'Own data' = 'own'))
             ),
             tags$div(id="2",
                      h1('Indicence Data'),
                      conditionalPanel("input.incidenceDataType == 'own'",
                             # State 2.1
                             fileInput('incidenceData', 'Choose incidence data file to upload',
                                       accept = c(
                                         'text/csv',
                                         'text/comma-separated-values',
                                         'text/tab-separated-values',
                                         'text/plain',
                                         '.csv',
                                         '.tsv'
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
                                          '"'),
                             sliderInput('uploadedWidth', 'Choose the width of the sliding time window for R estimation', min=1, max=20, value = 5)
                      ),
                      conditionalPanel("input.incidenceDataType == 'preloaded'",
                             # State 2.2
                             radioButtons('incidenceDataset', 'Choose your dataset',
                                          c('PennsylvaniaH1N12009', 'RotavirusGermany',
                                            'Flu1918', 'Flu2009', 'Measles1861', 'SARS2003', 'Smallpox1972')),
                             sliderInput('width', 'Choose the width of the sliding time window for R estimation', min=1, max=20, value = 5)
                       )
             ),
             tags$div(id="3",
                      # State 3.1
                      h1('Serial Interval (SI) Input'),
                      radioButtons('SIPatientData', 'Do you have Exposure Data to inform the SI?',
                                   c('Yes'='TRUE', 'No'='FALSE'))
             ),
             tags$div(id="4",
                      h1('Serial Interval (SI) Input'),
                      conditionalPanel("input.SIPatientData == 'TRUE'",
                            # State 4.1
                            radioButtons('SIDataType', 'Would you like to use an external file containing the exposure data, or a pre-loaded dataset?',
                                         c('Pre-loaded' = 'preloaded', 'Own data' = 'own'))
                       ),
                      conditionalPanel("input.SIPatientData == 'FALSE'", 
                           # State 4.2
                           radioButtons('uncertainty', 'Would you like to include SI uncertainty in your model?',
                                        c('Yes'='TRUE', 'No'='FALSE'))
                       )
             ),
             tags$div(id="5",
                      h1('Serial Interval (SI) Input'),
                      conditionalPanel("input.SIPatientData == 'TRUE' & input.SIDataType == 'preloaded'",
                            # State 5.1
                            radioButtons('SIDataset', 'Choose your dataset',
                                         c('PennsylvaniaH1N12009', 'RotavirusGermany'))
                       ),
                      conditionalPanel("input.SIPatientData == 'TRUE' & input.SIDataType == 'own'",
                             # State 5.2
                             radioButtons('SIFrom', 'Do you have raw exposure data or a SI posterior sample to upload?',
                                          c('Raw exposure data'='data', 'SI posterior sample'='sample'))
                       ),
                      conditionalPanel("input.SIPatientData == 'FALSE' & input.uncertainty == 'TRUE'",
                              # State 5.3
                              tags$div(class='final'),
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
                       ),
                      conditionalPanel("input.SIPatientData == 'FALSE' & input.uncertainty == 'FALSE'",
                             # State 5.4
                             radioButtons('parametric', 'Parametric on Non-Parametric SI?',
                                          c('Parametric'='TRUE', 'Non-Parametric'='FALSE'))
                      )
             ),
             tags$div(id="6",
                      h1('Serial Interval (SI) Input'),
                      conditionalPanel("input.SIPatientData == 'TRUE' & input.SIDataType == 'preloaded'",
                             # State 6.1
                             tags$div(class='final'),
                             radioButtons('SIDist', 'Choose your serial interval distribution',
                                          c('Gamma'='G',
                                            'Offset Gamma'='off1G',
                                            'Erlang' = 'E',
                                            'Weibull' = 'W',
                                            'Log-Normal' = 'L'))
                      ),
                      conditionalPanel("input.SIPatientData == 'TRUE' & input.SIDataType == 'own' & input.SIFrom == 'data'",
                               # State 6.2
                               
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
                                            '"')
                       ),
                      conditionalPanel("input.SIPatientData == 'TRUE' & input.SIDataType == 'own' & input.SIFrom == 'sample'",
                              # State 6.3
                              tags$div(class='final'),
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
                              radioButtons('SISep', 'Separator',
                                           c(Comma=',',
                                             Semicolon=';',
                                             Tab='\t'),
                                           ','),
                              radioButtons('SISampleQuote', 'Quote',
                                           c(None='',
                                             'Double Quote'='"',
                                             'Single Quote'="'"),
                                           '"'),
                              numericInput('n23', 'Choose n2', min=10, value=100)
                      ),
                      conditionalPanel("input.SIPatientData == 'FALSE' & input.uncertainty == 'FALSE' & input.parametric == 'TRUE'",
                               # State 6.4
                               tags$div(class='final'),
                               numericInput('Mean.SI2', 'Mean.SI', value=2, min=1+1e-18),
                               numericInput('Std.SI2', 'Std.SI', value=1, min=1e-18)
                      ),
                      conditionalPanel("input.SIPatientData == 'FALSE' & input.uncertainty == 'FALSE' & input.parametric == 'FALSE'",
                               # State 6.5
                               radioButtons('SIDistrDataType', 'Would you like to use an external file containing the SI distribution, or a pre-loaded SI distribution?',
                                            c('Pre-loaded' = 'preloaded', 'External file' = 'own'))

                      )
             ),
             tags$div(id="7",
                      h1('Serial Interval (SI) Input'),
                      conditionalPanel("input.SIPatientData == 'TRUE' & input.SIDataType == 'own' & input.SIFrom == 'data'",
                            # State 7.1
                            tags$div(class='final'),
                            radioButtons('SIDist2', 'Choose your serial interval distribution',
                                         c('Gamma'='G',
                                           'Offset Gamma'='off1G',
                                           'Erlang' = 'E',
                                           'Weibull' = 'W',
                                           'Log-Normal' = 'L')),
                            numericInput('n22', 'Choose n2', min=10, value=100),
                            p('NOTE: MCMC will run burnin + n1*thin iterations. This is slow. Try to keep below 10,000 even for small dataset'),
                            numericInput('n12', 'Choose the number of MCMC samples (n1)', min=10, value=500),
                            numericInput('burnin', 'Choose the number of MCMC burnin samples', min=0, value=3000),
                            numericInput('thin', 'Choose thin for MCMC, will run thin MCMC iterations for each sample', min=1, value=10),
                            numericInput('param1', 'Choose the value of param1 (MCMC init.pars)', min=0, value=''),
                            numericInput('param2', 'Choose the value of param1 (MCMC init.pars)', min=0, value='')
                      ),
                      conditionalPanel("input.SIPatientData == 'FALSE' & input.uncertainty == 'FALSE' & input.parametric == 'FALSE' & input.SIDistrDataType == 'own'",
                          # State 7.2    
                          tags$div(class='final'),
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
                                       '"')
                      ),
                      conditionalPanel("input.SIPatientData == 'FALSE' & input.uncertainty == 'FALSE' & input.parametric == 'FALSE' & input.SIDistrDataType == 'preloaded'",
                          # State 7.3
                          tags$div(class='final'),
                          radioButtons('SIDistrDataset', 'Choose your SI.Distr Dataset',
                                       c('Flu1918', 'Flu2009', 'Measles1861',
                                         'SARS2003', 'Smallpox1972'))
                      )
             ),
             tags$div(id="control",
                      actionButton('prev', label='Previous'),
                      actionButton('next', label='Next')
             )
    
      ),
     column(8, id="plot",
            plotOutput('plot')
      )
    
  )
)
