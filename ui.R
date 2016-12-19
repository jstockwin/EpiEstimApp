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
shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      radioButtons('data', 'Choose your dataset',
                   c('PennsylvaniaH1N12009', 'RotavirusGermany', 'Uploaded Data')),
      radioButtons('SIDist', 'Choose your serial interval distribution',
                   c('Gamma'='G',
                     'Offset Gamma'='off1G',
                     'Erlang' = 'E',
                     'Weibull' = 'W',
                     'Log-Normal' = 'L')),
      sliderInput('W', 'Choose a width:', min=1, max=20, value = 5),
      h1('Uploaded data'),
      p('The following settings are all regading the uploaded data set, and can be
        ignored if using the datasets provided by this application.'),
      tags$hr(),
      p('Below you can choose initial parameters for the MCMC fit of the uploaded data.
        Leaving either of them blank results in default behaviour, which should work in most cases.'),
      numericInput('param1', 'Choose the value of param1', min=0, value=''),
      numericInput('param2', 'Choose the value of param1', min=0, value=''),
      p('Below you can choose your csv files to upload. Please ensure the settings below these upload
        boxes are correct before uploading.'),
      fileInput('serialIntervalData', 'Choose serialIntervalData file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      fileInput('casesPerDayData', 'Choose casesPerDayData file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header', FALSE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    ),
    
    mainPanel(
      plotOutput('plot')
    )
  )
))
