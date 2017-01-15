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
          <p>TODO: Description here</p>
          "
        )      
      ),
      column(4, id="menu",
             tags$div(id="status",
                      verbatimTextOutput('output')
                      ),
             tags$div(id="1",
                      h3('Step 1 of 6'),
                      h1('Indicence Data'),
                      radioButtons('incidenceDataType', 'Would you like to use your own data, or a pre-loaded data set?',
                                   c('Pre-loaded' = 'preloaded', 'Own data' = 'own'))
             ),
             tags$div(id="2",
                      h3('Step 2 of 6'),
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
                             sliderInput('Width', 'Choose a width:', min=1, max=20, value = 5)
                      ),
                      conditionalPanel("input.incidenceDataType == 'preloaded'",
                             # State 2.2
                             radioButtons('incidenceDataset', 'Choose your dataset',
                                          c('PennsylvaniaH1N12009', 'RotavirusGermany')),
                             sliderInput('Width', 'Choose a width:', min=1, max=20, value = 5)
                       )
             ),
             tags$div(id="3",
                      # State 3.1
                      h3('Step 3 of 6'),
                      h1('Serial Interval Data'),
                      radioButtons('SIPatientData', 'Do you have Serial Inteval Patient Data?',
                                   c('Yes'='TRUE', 'No'='FALSE'))
             ),
             tags$div(id="4",
                      h3('Step 4 of 6'),
                      h1('Serial Interval Data'),
                      conditionalPanel("input.SIPatientData == 'TRUE'",
                            # State 4.1
                            radioButtons('SIDataType', 'Would you like to use your own data, or a pre-loaded data set?',
                                         c('Pre-loaded' = 'preloaded', 'Own data' = 'own'))
                       ),
                      conditionalPanel("input.SIPatientData == 'FALSE'", 
                           # State 4.2
                           radioButtons('uncertainty', 'Would you like to include uncertainty in your model?',
                                        c('Yes'='TRUE', 'No'='FALSE'))
                       )
             ),
             tags$div(id="5",
                      h3('Step 5 of 6'),
                      conditionalPanel("input.SIPatientData == 'TRUE' & input.SIDataType == 'preloaded'",
                            # State 5.1
                            radioButtons('SIDataset', 'Choose your dataset',
                                         c('PennsylvaniaH1N12009', 'RotavirusGermany'))
                       ),
                      conditionalPanel("input.SIPatientData == 'TRUE' & input.SIDataType == 'own'",
                             # State 5.2
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
                      conditionalPanel("input.SIPatientData == 'FALSE' & input.uncertainty == 'TRUE'",
                              # State 5.3
                              p('You are in state 5.3, which is TODO')
                       ),
                      conditionalPanel("input.SIPatientData == 'FALSE' & input.uncertainty == 'FALSE'",
                             # State 5.4
                             radioButtons('parametric', 'Parametric on Non-Parametric?',
                                          c('Parametric'='TRUE', 'Non-Parametric'='FALSE'))
                      )
             ),
             tags$div(id="6",
                      h3('Step 6 of 6'),
                      conditionalPanel("input.SIPatientData == 'TRUE' & input.SIDataType == 'preloaded'",
                             # State 6.1
                             radioButtons('SIDist', 'Choose your serial interval distribution',
                                          c('Gamma'='G',
                                            'Offset Gamma'='off1G',
                                            'Erlang' = 'E',
                                            'Weibull' = 'W',
                                            'Log-Normal' = 'L'))
                      ),
                      conditionalPanel("input.SIPatientData == 'TRUE' & input.SIDataType == 'own'",
                              # State 6.2
                              radioButtons('SIDist', 'Choose your serial interval distribution',
                                           c('Gamma'='G',
                                             'Offset Gamma'='off1G',
                                             'Erlang' = 'E',
                                             'Weibull' = 'W',
                                             'Log-Normal' = 'L')),
                              numericInput('param1', 'Choose the value of param1', min=0, value=''),
                              numericInput('param2', 'Choose the value of param1', min=0, value='')
                      ),
                      conditionalPanel("input.SIPatientData == 'FALSE' & input.uncertainty == 'TRUE'",
                               # State 6.3
                               p('You are in state 6.3, which is TODO')
                      ),
                      conditionalPanel("input.SIPatientData == 'FALSE' & input.uncertainty == 'FALSE' & input.parametric == 'TRUE'",
                               # State 6.4
                               p('You are in state 6.4, which is TODO')
                      ),
                      conditionalPanel("input.SIPatientData == 'FALSE' & input.uncertainty == 'FALSE' & input.parametric == 'FALSE'",
                               # State 6.5
                               p('You are in state 6.5, which is TODO')
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
