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
      column(12,
        HTML(
          "
          <h1>Transmissibility Estimator</h1>
          <p>TODO: Description here</p>
          "
        )      
      ),
      column(4, id="menu",
      actionButton('go', label='Go'),
      actionButton('stop', label='Stop'),
      verbatimTextOutput('output'),
      radioButtons('data', 'Choose your dataset',
                   c('PennsylvaniaH1N12009', 'RotavirusGermany', 'Uploaded Data')),
      radioButtons('SIDist', 'Choose your serial interval distribution',
                   c('Gamma'='G',
                     'Offset Gamma'='off1G',
                     'Erlang' = 'E',
                     'Weibull' = 'W',
                     'Log-Normal' = 'L')),
      sliderInput('W', 'Choose a width:', min=1, max=20, value = 5),
      conditionalPanel(condition="input.data == 'Uploaded Data'",
         h1('Uploaded data'),
         p('The following settings are all regading the uploaded data set, and can be
           ignored if using the datasets provided by this application.'),
         tags$hr(),
         p('Below you can choose initial parameters for the MCMC fit of the uploaded data.
           Leaving either of them blank results in default behaviour, which tries 1,000 random
           parameters between 0 and 100 until it finds some that work.'),
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
     ) # Close conditional panel
     
      
    
      ),
     column(8, id="plot",
            plotOutput('plot')
            ),
    

    
    
    tags$script('
    var run = false;
    var go = document.getElementById("go");
    var stop = document.getElementById("stop");
    var output = document.getElementById("output");
    output.innerText = "Initialising...";
    var i = 0;
    go.onclick = function() {
      var i = 0;
      run = true;
      go.disabled = true;
      go.innerText = "Running";
      stop.disabled = false;
      Shiny.onInputChange("mydata", "NEW");
      output.innerText = "Running... 0%";
    };
    document.getElementById("stop").onclick = function() {
      stop.innerText = "Stopping";
      stop.disabled = true;
      run = false;
      Shiny.onInputChange("mydata", "STOP");
    };
    Shiny.addCustomMessageHandler("done", function(data) {
          go.disabled = false;
          stop.diabled = true;
          stop.innerText = "Stop";
          go.innerText = "Go";
          output.innerText = "Ready";
    })
    Shiny.addCustomMessageHandler("pingToClient",     
      function(data) {
        if (run) {
          // If we are still running, ping data straight back to client
          // but pause for a little first to keep the R thread free
          i += 1
          output.innerText = "Running... " + Math.round(i) + "%";
          setTimeout(Shiny.onInputChange("mydata", data), 10);
        } else {
          // Else, stop.
          go.disabled = false;
          stop.diabled = true;
          stop.innerText = "Stop";
          go.innerText = "Go";
        }
      }
    );
    ')
    
  )
)
