# Define UI for application that draws a histogram
shiny::shinyUI(shiny::bootstrapPage(theme = shinythemes::shinytheme("spacelab"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "shortcut icon", type = "image/x-icon",
              href = "favicon.ico")
  ),
  # include the js code
  shinyjs::useShinyjs(),
  shiny::includeScript("scripts.js"),
  column(12,
         HTML(
           "
          <meta name='keywords' content='infectious,disease,epidemiology,transmissibility,serial,interval,generation,time,time,varying,reproduction,number,Robin,Thompson,Anne,Cori,Jake,Stockwin,Hackout3'>
          <meta name='author' content='Jake Stockwin'>
          <h1>EpiEstim App</h1>
          <p>This web application generates an estimate of infectious disease transmissibility throughout an outbreak. The time-dependent reproduction number (<i>R</i>) is inferred from disease incidence time series and patient data or estimates of the serial interval. For more information about how to use this application, please see the <a href='https://github.com/jstockwin/EpiEstimApp/wiki'>wiki</a>. For the most part, we assume that users will want to upload their own data, however there are also some <a href='https://github.com/jstockwin/EpiEstimApp/wiki/Preloaded-Datasets' target='_blank'>example datasets</a> built in, to illustrate how the application might be used.</p>
          <p>For more information on uploading your own data and on the required format of the data, please
          <a href='https://github.com/jstockwin/EpiEstimApp/wiki/Uploading-your-own-data' target='_blank'>click here</a>.</p>
          <p>The authors request users to cite the original publication when referring to this tool, any results generated from it, or the R software application on which this tool is based (EpiEstim 2):

</p>
          <p>Thompson RN, Stockwin JE, van Gaalen RD, Polonsky JA, et al. Improved inference of time-varying reproduction numbers during infectious disease outbreaks. Submitted (2018).</p>
          "
         )
  ),
  column(4, id = "menu",
         div(id = "status",
             shiny::verbatimTextOutput("output"),
             shiny::htmlOutput("progress")
         ),
         div(id = "titles",
             div(id = "incidence_title", h1("Incidence Data")),
             shinyjs::hidden(div(id = "si_title",
                                 h1("Serial Interval (SI) Input")))
          ),
         div(id = "1.1",
             div(id = "incidence_data_type_error_box", class = "error_box",
               shiny::radioButtons("incidence_data_type",
                            paste("Do you want to use pre-loaded incidence",
                                  "time series data or upload your own?"),
                            c("Pre-loaded" = "preloaded", "Own data" = "own"))
             )
         ),
         shinyjs::hidden(div(id = "2.1",
                    # State 2.1
                    h2("R Estimation Settings"),
                    div(id = "incidence_data_error_box", class = "error_box",
                        shiny::fileInput("incidence_data",
                                  "Choose incidence data file to upload",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values",
                                    "text/plain",
                                    ".csv"
                                  )
                        )
                      ),
                    shiny::checkboxInput("incidence_header", "Header", FALSE),
                    div(id = "uploaded_width_error_box", class = "error_box",
                        shiny::sliderInput("uploaded_width",
                                           paste("Choose the length of the",
                                                 "sliding time window, W, over which R",
                                                 "is estimated"),
                                           min = 1, max = 20, value = 7)
                    ),
                    div(id = "uploaded_mean_prior_error_box",
                        class = "error_box",
                        shiny::numericInput("uploaded_mean_prior",
                                            "Choose the prior mean value for R",
                                            value = 5, min = 0)
                    ),
                    div(id = "uploaded_std_prior_error_box",
                        class = "error_box",
                        shiny::numericInput("uploaded_std_prior",
                                            paste("Choose the prior standard",
                                                  "deviation value for R"),
                                            value = 5, min = 0)
                    )
         )),
         shinyjs::hidden(div(id = "2.2",
                    # State 2.2
                    h2("R Estimation Settings"),
                    div(id = "incidence_dataset_error_box", class = "error_box",
                        shiny::radioButtons("incidence_dataset",
                                            "Choose your dataset",
                                             c(
                                               "H1N1Pennsylvania2009",
                                               "H1N1NewYork2009",
                                               "RotavirusKiribati2013",
                                               "H1N1Maryland1918",
                                               "MeaslesGermany1861",
                                               "SARSHongKong2003",
                                               "SmallpoxKosovo1972"
                                              )
                                            )
                    ),
                    div(id = "incidence_width_error_box", class = "error_box",
                        shiny::sliderInput("incidence_width",
                                           paste("Choose the length of the",
                                                 "sliding time window, W, over which R",
                                                 "is estimated"),
                                           min = 1, max = 20, value = 7)
                    ),
                    div(id = "incidence_mean_prior_error_box",
                        class = "error_box",
                        shiny::numericInput("incidence_mean_prior",
                                            "Choose the prior mean value for R",
                                            value = 5, min = 0)
                    ),
                    div(id = "incidence_std_prior_error_box",
                        class = "error_box",
                        shiny::numericInput("incidence_std_prior",
                                            paste("Choose the prior standard",
                                                  "deviation value for R"),
                                            value = 5, min = 0)
                    )
         )),
         shinyjs::hidden(div(id = "3.1",
                    # State 3.1
                    div(id = "imported_error_box", class = "error_box",
                        shiny::radioButtons("imported",
                                   paste("Do you have data about which",
                                         "cases are imported?"),
                                   c("No" = "FALSE", "Yes" = "TRUE"))
                    )
         )),
         shinyjs::hidden(div(id = "4.1",
                    # State 4.1
                    div(id = "imported_data_error_box", class = "error_box",
                        shiny::fileInput("imported_data",
                                         paste("Choose a data file consisting of",
                                               "numbers of imported cases to",
                                               "upload"),
                                         accept = c(
                                           "text/csv",
                                           "text/comma-separated-values",
                                           "text/plain",
                                           ".csv"
                                         )
                      )
                    ),
                    shiny::checkboxInput("imported_header", "Header", FALSE)
         )),
         shinyjs::hidden(div(id = "5.1",
                    # State 5.1
                    div(id = "si_patient_data_error_box", class = "error_box",
                        shiny::radioButtons("si_patient_data",
                                            paste("Do you want to use a",
                                                  "distributional estimate of the",
                                                  "serial interval, or use",
                                                  "data from patients",
                                                  "in known transmission chains?"),
                                   c("Distributional Estimate" = "FALSE",
                                     "Patient Data" = "TRUE"))
                    )
         )),
         shinyjs::hidden(div(id = "6.1",
                    # State 6.1
                    div(id = "si_data_type_error_box", class = "error_box",
                        shiny::radioButtons("si_data_type",
                                            paste("Would you like to use an in-built example dataset
                                                  containing exposure data, or upload",
                                                  "your own dataset?"),
                                   c("Pre-loaded" = "preloaded",
                                     "Own data" = "own"))
                    )
         )),
         shinyjs::hidden(div(id = "6.2",
                    # State 6.2
                    div(id = "si_est_type_error_box", class = "error_box",
                        shiny::radioButtons("si_est_type",
                        paste("Which of the following types of serial interval",
                              "distributions would you like to use?"),
      c(
        "Parametric with uncertainty (offset gamma)" = "uncertain",
        "Parametric without uncertainty (offset gamma)" = "parametric",
        "Upload a probability distribution" = "own",
        "Use a distribution estimated from a previous outbreak (data in-built in app)"
        = "preloaded"
        )
                      )
                    )
         )),
         shinyjs::hidden(div(id = "7.1",
                    # State 7.1
                    p("Please note that these preloaded datasets are only to provide examples as to how the app might be used. As such, the full range of options is not provided here (e.g. the number of steps in the MCMC chain is fixed)."), #nolint (can't split html)
                    HTML("<p>If you would like to have the full range of options, please download the serial interval data for these datasets from <a href='https://github.com/jstockwin/EpiEstimApp/tree/master/inst/app/datasets/SerialIntervalData' target='_blank'>here</a>, then go back a step and select 'Own data' followed by 'Patient data'.</p>"), #nolint can't split html
                    div(id = "si_dataset_error_box", class = "error_box",
                        shiny::radioButtons("si_dataset", "Choose your dataset",
                                   c("RotavirusEcuador2011", "H1N1NewYork2009",
                                     "H1N1USA2009"))
                    )
         )),
         shinyjs::hidden(div(id = "7.2",
                    # State 7.2
                    div(id = "si_from_error_box", class = "error_box",
                        shiny::radioButtons("si_from",
                                            paste("Do you have data from patients",
                                                  "in known transmission chains",
                                                  "or a SI posterior",
                                                  "sample to upload?"),
                                            c("Patient data" = "data",
                                              "SI posterior sample" = "sample"
                                              )
                                            )
                    )
         )),
         shinyjs::hidden(div(id = "7.3",
                    # State 7.3
                    div(id = "n1_error_box", class = "error_box",
                        shiny::numericInput("n1", "n1", min = 2, value = 50),
                      p("Choose the number of serial interval distributions (n1) to be sampled (see parameters below)") #nolint (can't split html)
                    ),
                    div(id = "n2_error_box", class = "error_box",
                        shiny::numericInput("n2", "n2", min = 2, value = 50),
                      p("Choose the number of serial interval values (n2) to be sampled from each of the n1 serial interval distributions in order to estimate R") #nolint (can't split html)
                    ),
                    tags$hr(),
                    tags$p("Choose values describing the mean serial interval distribution, and the uncertainty around these values"), #nolint (can't split html)
                    div(id = "mean_si_error_box", class = "error_box",
                        shiny::numericInput("mean_si", "mean_si",
                                            value = 2, min = 1)
                    ),
                    div(id = "std_mean_si_error_box", class = "error_box",
                        shiny::numericInput("std_mean_si", "std_mean_si",
                                            min = 0, value = 1)
                    ),
                    div(id = "min_mean_si_error_box", class = "error_box",
                        shiny::numericInput("min_mean_si", "min_mean_si",
                                            min = 1, value = 1)
                    ),
                    div(id = "max_mean_si_error_box", class = "error_box",
                        shiny::numericInput("max_mean_si", "max_mean_si",
                                            value = 3, min = 1)
                    ),
                    div(id = "std_si_error_box", class = "error_box",
                        shiny::numericInput("std_si", "std_si",
                                            value = 2, min = 1)
                    ),
                    div(id = "std_std_si_error_box", class = "error_box",
                        shiny::numericInput("std_std_si", "std_std_si",
                                            min = 0, value = 1)
                    ),
                    div(id = "min_std_si_error_box", class = "error_box",
                        shiny::numericInput("min_std_si", "min_std_si",
                                            value = 1, min = 1)
                    ),
                    div(id = "max_std_si_error_box", class = "error_box",
                        shiny::numericInput("max_std_si", "max_std_si",
                                            value = 3, min = 1)
                    ),
                    div(id = "uncertain_seed_error_box", class = "error_box",
                        shiny::numericInput("uncertain_seed",
                                            paste("Set a seed to be used by",
                                                  "EpiEstim, so that the",
                                                  "results are reproducible.",
                                                  "A random seed will be",
                                                  "chosen if this is left",
                                                  "blank"), value = NULL)
                    )
         )),
         shinyjs::hidden(div(id = "7.4",
                    # State 7.4
                    div(id = "mean_si2_error_box", class = "error_box",
                        shiny::numericInput("mean_si2", "mean_si",
                                            value = 2, min = 1 + 1e-18)
                    ),
                    div(id = "std_si2_error_box", class = "error_box",
                        shiny::numericInput("std_si2", "std_si",
                                            value = 1, min = 1e-18)
                    )
         )),
         shinyjs::hidden(div(id = "7.5",
                    # State 7.5
                    div(id = "si_distr_data_error_box", class = "error_box",
                        shiny::fileInput("si_distr_data",
                                  "Choose serial interval data file to upload",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values",
                                  "text/plain",
                                  ".csv"
                                )
                      )
                    ),
                    shiny::checkboxInput("si_distr_header", "Header", FALSE)
         )),
         shinyjs::hidden(div(id = "7.6",
                    # State 7.6
                    div(id = "si_distr_dataset_error_box", class = "error_box",
                        shiny::radioButtons("si_distr_dataset",
                                            "Choose the dataset corresponding to the serial interval distribution you would like to use",
                                   c("H1N1Maryland1918", "H1N1Pennsylvania2009",
                                     "MeaslesGermany1861", "SARSHongKong2003",
                                     "SmallpoxKosovo1972"))
                    )
         )),
         shinyjs::hidden(div(id = "8.1",
                    # State 8.1
                    div(id = "si_dist_error_box", class = "error_box",
                        shiny::radioButtons("si_dist",
                                     "Choose your serial interval distribution",
                                     c("Gamma" = "G",
                                       "Offset Gamma" = "off1G",
                                       "Weibull" = "W",
                                       "Offset Weibull" = "off1W",
                                       "Log-Normal" = "L",
                                       "Offset Log-Normal" = "off1L"))
                        ),
                    div(id = "n24_error_box", class = "error_box",
                        shiny::numericInput("n24",
                                            paste("Choose n2, the posterior",
                                                  "sample size to be drawn for",
                                                  "R for each SI distribution",
                                                  "sampled"),
                                            min = 10, value = 100)
                        ),
                    div(id = "preloaded_seed_error_box", class = "error_box",
                        shiny::numericInput("preloaded_seed",
                                            paste("Set a seed to be used by",
                                                  "EpiEstim. A random seed will",
                                                  "be chosen if this is left",
                                                  "blank"),
                                            value = NULL)
                        )
         )),
         shinyjs::hidden(div(id = "8.2",
                    # State 8.2
                    div(id = "si_data_error_box", class = "error_box",
                        shiny::fileInput("si_data",
                                "Choose serial interval data file to upload",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values",
                                  "text/plain",
                                  ".csv"
                                )
                      )
                    ),
                    shiny::checkboxInput("si_header", "Header", FALSE),
                    div(id = "uploaded_si_seed_error_box", class = "error_box",
                        shiny::numericInput("uploaded_si_seed",
                                            paste("Set a seed to be used by",
                                                  "EpiEstim. A random seed will",
                                                  "be chosen if this is left",
                                                  "blank"),
                                            value = NULL)
                    )
         )),
         shinyjs::hidden(div(id = "8.3",
                    # State 8.3
                    div(id = "si_sample_data_error_box", class = "error_box",
                        shiny::fileInput("si_sample_data",
                                "Choose serial interval data file to upload",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values",
                                  "text/plain",
                                  ".csv"
                                )
                      )
                    ),
                    shiny::checkboxInput("si_sample_header", "Header", FALSE),
                    div(id = "n23_error_box", class = "error_box",
                        shiny::numericInput("n23",
                                            paste("Choose n2, the posterior",
                                                  "sample size to be drawn for",
                                                  "R for each SI distribution",
                                                  "sampled"),
                                            min = 10, value = 100)
                    ),
                    div(id = "si_sample_seed_error_box", class = "error_box",
                        shiny::numericInput("si_sample_seed",
                                            paste("Set a seed to be used by",
                                                  "EpiEstim. A random seed will",
                                                  "be chosen if this is left",
                                                  "blank"),
                                            value = NULL)
                    )
         )),
         shinyjs::hidden(div(id = "9.1",
                    # State 9.1
                    div(id = "si_dist_2_error_box", class = "error_box",
                        shiny::radioButtons("si_dist_2",
                                   "Choose your serial interval distribution",
                                   c("Gamma" = "G",
                                     "Offset Gamma" = "off1G",
                                     "Weibull" = "W",
                                     "Offset Weibull" = "off1W",
                                     "Log-Normal" = "L",
                                     "Offset Log-Normal" = "off1L"))
                    ),
                    p("NOTE: MCMC will run burnin + n1*thin iterations. This is slow. Try to keep below 10,000 even for small datasets. For longer MCMC chains, please use the EpiEstim 2.0 R package directly, rather than our online interface."), #nolint (can't split html)
                    div(id = "n12_error_box", class = "error_box",
                        shiny::numericInput("n12",
                                            paste("Choose the number of serial",
                                                  "interval distributions to",
                                                  "be estimated using MCMC",
                                                  "(n1)"),
                                            min = 10, value = 500)
                    ),
                    div(id = "burnin_error_box", class = "error_box",
                        shiny::numericInput("burnin",
                                            paste("Choose the number of",
                                                  "iterations used as MCMC",
                                                  "burn in"),
                                            min = 0, value = 3000)
                    ),
                    div(id = "thin_error_box", class = "error_box",
                        shiny::numericInput("thin",
                                            paste("Choose the MCMC thinning",
                                                  "parameter (thin). Each of",
                                                  "the n1 estimated serial",
                                                  "interval distributions will",
                                                  "be taken after every 'thin'",
                                                  "iterations of the MCMC",
                                                  "chain."),
                                            min = 1, value = 10)
                    ),
                    div(id = "n22_error_box", class = "error_box",
                        shiny::numericInput("n22",
                                            paste("Choose the number of",
                                                  "serial interval values to",
                                                  "be sampled from each",
                                                  "estimated serial interval",
                                                  "distribution in order to",
                                                  "estimate R (n2)"),
                                            min = 10, value = 100)
                    ),
                    div(id = "mcmc_seed_error_box", class = "error_box",
                        shiny::numericInput("mcmc_seed",
                                            paste("Set a seed for the MCMC. If",
                                                  "no seed is chosen, a random",
                                                  "value will be selected."),
                                            value = NULL)
                    ),
                    tags$hr(),
                    p("The following two parameters give the initial parameters to use in MCMC. For Gamma, Offset Gamma and Weibull distributions, param1 is the shape and param2 is the scale of the distribution. For Log-Normal, param1 and param2 are respectively the mean and standard deviation of the logarithm."), #nolint can't split html
                    div(id = "param1_error_box", class = "error_box",
                        shiny::numericInput("param1",
                                            "Choose the value of param1",
                                            min = 0, value = "")
                    ),
                    div(id = "param2_error_box", class = "error_box",
                        shiny::numericInput("param2",
                                            "Choose the value of param2",
                                            min = 0, value = "")
                    )
         )),
         div(id = "control",
             shinyjs::hidden(actionButton("stop", label = "Stop")),
             shinyjs::disabled(actionButton("prev", label = "Previous")),
             shinyjs::disabled(actionButton("nxt", label = "Next")),
             shinyjs::hidden(actionButton("go", label = "Go")),
             textOutput("error")
         )
         
  ),
  column(8, id = "plot",
         tabsetPanel(
           tabPanel("Plot",
                    downloadButton("save_plot", "Save Image"),
                    plotOutput("plot")
           ),
           tabPanel("Incidence Data",
                    downloadButton("save_incidence",
                                   "Save Table"),
                    tableOutput("incidence_data_output")
           ),
           tabPanel("Estimated Reproduction Number",
                    downloadButton("save_r", "Save Table"),
                    tableOutput("estimated_r_output")
           ),
           tabPanel("Serial Interval Distribution",
                    downloadButton("save_si", "Save Table"),
                    tableOutput("serial_interval_output")
           )
         )
  )
  
)
)
