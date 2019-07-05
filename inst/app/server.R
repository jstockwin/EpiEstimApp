future::plan(future::multiprocess)


# Source necessary files
source("utils.R")

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9 * 1024 ^ 2)
options(shiny.reactlog = TRUE)

all_states <- c("1.1", "2.1", "2.2", "3.1", "4.1", "5.1", "6.1", "6.2", "7.1",
              "7.2", "7.3", "7.4", "7.5", "7.6", "8.1", "8.2", "8.3", "9.1")

final_states <- c("7.3", "7.4", "7.5", "7.6", "8.1", "8.3", "9.1")

# If the app has crashed we may be left with MCMC progress files, which would
# throw off our counts of how many MCMC processes are running.
# To be sure this doesn't happen, we will clear the repsective folders
# when this happens.
logdir <- tempfile()
mcmc_progress_folder <- paste(logdir, "mcmc-progress/progress/", sep="/")
dir.create(mcmc_progress_folder, recursive = TRUE)
mcmc_pid_folder <- paste(logdir, "mcmc-progress/pid/", sep="/")
dir.create(mcmc_pid_folder, recursive = TRUE)

if (length(list.files(path = mcmc_progress_folder, pattern = "*.txt")) > 0) {
  file.remove(paste(mcmc_progress_folder,
                    list.files(path = mcmc_progress_folder, pattern = "*.txt"),
                    sep = ""))
}
if (length(list.files(path = mcmc_pid_folder, pattern = "*.txt")) > 0) {
  file.remove(paste(mcmc_pid_folder, list.files(path = mcmc_pid_folder,
                                              pattern = "*.txt"), sep = ""))
}

shiny::shinyServer(function(input, output, session) {
  # Initialise some reactive values
  t <- as.numeric(Sys.time())
  id <- 1e8 * (t - floor(t))
  id <- gsub("\\.", "-", as.character(id))
  progress_file <- paste(mcmc_progress_folder, id, #nolint
                         "-progress.txt", sep = "")
  pid_file <- paste(mcmc_pid_folder, id, "-pid.txt", sep = "") #nolint
  values <- shiny::reactiveValues(state = "1.1", status = "Ready", error = NULL)
  shinyjs::enable("nxt") # enable next button when initial load is done.
  async_data <-
    shiny::reactiveValues(epi_estim_output = NULL, mcmc_samples = NULL,
                   si_sample_from_data = NULL, convergence_check = NULL)
  async_data_being_loaded <- list()
  # Initialise inputs for EpiEstim's estimate_R
  config <- EpiEstim::make_config(list(
    n2 = 100,
    mcmc_control = EpiEstim::make_mcmc_control(
      burnin = 3000
    ),
    mean_prior = 5,
    std_prior = 5
  ))
  method <- NULL
  si_sample <- NULL
  si_data <- NULL
  incidence_data <- NULL
  requested_seed <- NULL
  requested_mcmc_seed <- NULL
  total_samples_needed <- 1 # Will be overridden.
              # Set to 1 so dim(mcmc_samples) < total_samples_needed initially
  mcmc_samples <- NULL #nolint
  si_sample_from_data <- NULL
  convergence_check <- NULL
  si_distr_data <- NULL

  # Clicking previous/next should increment the stateLevel
  shiny::observeEvent(input$nxt, {
    # WARNING: You probably want to avoid much logic here. Most of it should
    # be in handle_state() which is reactive.
    # If Next is pressed twice without inputs changing, nothing will happen,
    # but if anything you put here WILL get done.
    if (handle_state()) {
      values$state <- get_next_state(values$state)
      if (values$state == "5.1") {
        shinyjs::hide("incidence_title")
        shinyjs::show("si_title")
      }
      values$status <- "Ready"
    }
  })
  shiny::observeEvent(input$prev, {
    if (values$state == "5.1") {
      shinyjs::show("incidence_title")
      shinyjs::hide("si_title")
    }
    values$state <- get_prev_state(values$state)
    values$error <- NULL
    session$sendCustomMessage(type = "reset_error_boxes", "") #nolint
  })

  # Whenever the state changes, toggle which fields are/are not visible.
  shiny::observe({
    for (some_state in all_states) {
      shinyjs::toggle(some_state, condition = some_state == values$state)
    }
    # hide/show next button as appropriate
    shinyjs::toggle("nxt", condition = !(values$state %in% final_states))#nolint
    # hide/show go button as appropriate
    shinyjs::toggle("go", condition = values$state %in% final_states) #nolint
    # Disable/enable prev button as appropriate
    shinyjs::toggleState("prev", condition = !(values$state == "1.1"))
  })

  # Keep the output text to values$status
  output$output <- shiny::renderText({values$status}) #nolint
  output$progress <- shiny::renderText({
    paste(
      "<p>Step ", substr(values$state, 1, 1), " of at most 9.",
      " View the <a href='https://github.com/jstockwin/EpiEstimApp/",
      "wiki/Interactive-Documentation-State-", values$state,
      "' target='_blank'>interactive documentation</a> for this state.</p>"
      , sep = "")
    })
  output$error <- shiny::renderText({values$error}) #nolint



  # Logic for when "go" is clicked.
  shiny::observeEvent(input$go, {
    # Clear the current EpiEstim data (will remove current plot)
    async_data$epi_estim_output <- NULL
    run()
    })

  run <- function() {
    #TODO - can we make this async? Might be a bit more tricky...
    # WARNING: You probably want to avoid much logic here. Most of it should
    # be in handle_state() which is reactive.
    # If Next is pressed twice without inputs changing, nothing will happen,
    # but if anything you put here WILL get done.
    tryCatch({
      if (handle_state()) {
        # The following sets a seed randomly if no seed was requested.
        # We're using the "requested" seed to ensure that if no seed is
        # requested then a NEW random seed is chosen on EACH run.
        if (is.null(requested_seed) || is.na(requested_seed)) {
            t <- as.numeric(Sys.time())
            config$seed <- 1e8 * (t - floor(t))
        } else {
            config$seed <- requested_seed
        }

        if (method == "si_from_data" && is.null(mcmc_samples)) {
          values$status <- "Running MCMC (0%)"
          start_async_data_load("mcmc_samples", future::future({
            # The following sets a seed randomly if no seed was requested.
            # We're using the "requested" seed to ensure that if no seed is
            # requested then a NEW random seed is chosen on EACH run.
            if (is.null(requested_mcmc_seed) || is.na(requested_mcmc_seed)) {
                t <- as.numeric(Sys.time())
                mcmc_seed <- 1e8 * (t - floor(t))
            } else {
                mcmc_seed <- requested_mcmc_seed #nolint
            }
            if (.Platform$OS.type == "unix") {
              write(Sys.getpid(), file = pid_file)
            }
            capture.output(
              samples <- coarseDataTools::dic.fit.mcmc(
                dat = si_data,
                dist = config$si_parametric_distr,
                init.pars = config$mcmc_control$init_pars,
                burnin = config$mcmc_control$burnin,
                n.samples = config$n1 * config$mcmc_control$thin,
                verbose = floor(total_samples_needed / 100),
                seed = mcmc_seed)@samples,
              file = progress_file
            )
            file.remove(progress_file)
            if (.Platform$OS.type == "unix") {
              file.remove(pid_file)
            }
            return(samples)
          }))
        } else {
          if (method == "si_from_data") {
            # We have a full set of samples.
            mcmc_samples <- async_data$mcmc_samples

            if (is.null(si_sample_from_data)) {
              values$status <- "Running coarse2estim"
              start_async_data_load("si_sample_from_data", future::future({
                  EpiEstim::coarse2estim(samples = mcmc_samples,
                               dist = config$si_parametric_distr,
                               thin = config$mcmc_control$thin)$si_sample
              }))
            } else if (is.null(convergence_check)) {
              values$status <- "Running the Gelman-Rubin convergence check"
              start_async_data_load("convergence_check", future::future({
                if (.Platform$OS.type == "unix") {
                  write(Sys.getpid(), file = pid_file)
                }
                ret <- EpiEstim::check_cdt_samples_convergence(mcmc_samples)
                if (.Platform$OS.type == "unix") {
                  file.remove(pid_file)
                }
                return(ret)
              }))
            } else {
              # Good to go!
              # Run si_from_sample not si_from_data using si_sample_from_data
              # (which is the result of us running MCMC)
              # The whole thing is equivalent to passing si_data to
              # estimate_R(method="si_from_data"), but this way we
              # get a progress bar.
              if (!convergence_check) {
                # FYI: This works in browsers, but seems to stop everything
                # when done in RStudio
                session$sendCustomMessage(type = "alert", #nolint
                    "Warning: The Gelan-Rubin algorithm suggests that MCMC may",
                    "not have converged within the number of iterations",
                    "sepcified (burnin + n1*thin). estimate_R will be called",
                    "anyway, but you should investigate this issue.")
              }
              values$status <- "Running estimate_R..."
              start_async_data_load("epi_estim_output", future::future({
                if (.Platform$OS.type == "unix") {
                  write(Sys.getpid(), file = pid_file)
                }
                ret <- EpiEstim::estimate_R(incidence_data,
                                           method = "si_from_sample",
                                           si_sample = si_sample_from_data,
                                           config = config)
                if (.Platform$OS.type == "unix") {
                  file.remove(pid_file)
                }
                return(ret)
              }))
           }
          } else {
            start_async_data_load("epi_estim_output", future::future({
              if (.Platform$OS.type == "unix") {
                write(Sys.getpid(), file = pid_file)
              }
              ret <- EpiEstim::estimate_R(incidence_data, method = method,
                               si_data = si_data, si_sample = si_sample,
                               config = config)
              if (.Platform$OS.type == "unix") {
                file.remove(pid_file)
              }
              return(ret)
            }))
          }
        }

      }
    },
    error = function (e) {
      shinyjs::show("prev")
      shinyjs::hide("stop")
      shinyjs::enable("go")
      handle_error(values$state, e)
    })
  }

  output$plot <- shiny::renderPlot({
    if (!is.null(async_data$epi_estim_output)) {
      plot(async_data$epi_estim_output)
      values$status <- "Ready"
      shinyjs::show("prev")
      shinyjs::hide("stop")
      shinyjs::enable("go")
    }
  })

  output$savePlot <- shiny::downloadHandler(
    filename = function() {"Plot.png"}, #nolint
    content = function(file) {
      if (!is.null(async_data$epi_estim_output)){
        png(file)
        print(plot(async_data$epi_estim_output))
        dev.off()
      }
    }
  )

  output$incidence_data_output <- shiny::renderTable({
    if (!is.null(async_data$epi_estim_output)) {
        local <- round(async_data$epi_estim_output$I_local, 2)
        imported <- round(async_data$epi_estim_output$I_imported, 2)
        values$status <- "Ready"
        shinyjs::show("prev")
        shinyjs::hide("stop")
        shinyjs::enable("go")
        data.frame(local, imported)
    }
  })

  output$save_incidence <- shiny::downloadHandler(
    filename = function() {"incidence_data.csv"}, #nolint
    content = function(file) {
      if (!is.null(async_data$epi_estim_output)) {
        local <- async_data$epi_estim_output$I_local
        imported <- async_data$epi_estim_output$I_imported
        dat <- data.frame(local, imported)
        write.csv(dat, file, row.names = FALSE)
      }
    }
  )

  output$save_si <- shiny::downloadHandler(
    filename = function() {"SerialIntervalEstimates.csv"}, #nolint
    content = function(file) {
      if (!is.null(async_data$epi_estim_output)) {
        write.csv(async_data$epi_estim_output$si_distr, file, row.names = FALSE)
      }
    }
  )

  output$estimated_r_output <- shiny::renderTable({
    if (!is.null(async_data$epi_estim_output)) {
      values$status <- "Ready"
      shinyjs::show("prev")
      shinyjs::hide("stop")
      shinyjs::enable("go")
      round(async_data$epi_estim_output$R, 2)
    }
  })

  output$serial_interval_output <- shiny::renderTable({
    if (!is.null(async_data$epi_estim_output)) {
      values$status <- "Ready"
      shinyjs::show("prev")
      shinyjs::hide("stop")
      shinyjs::enable("go")
      round(async_data$epi_estim_output$si_distr, 2)
    }
  })

  output$save_r <- shiny::downloadHandler(
    filename = function() {"EstimatedR.csv"}, #nolint
    content = function(file) {
      if (!is.null(async_data$epi_estim_output)) {
        write.csv(async_data$epi_estim_output$R, file, row.names = FALSE)
      }
    }
  )


  handle_state <- function() {
    # Run when next is clicked. Should handle all validation and error checks
    # for that state and should set all necessary variables.
    # The state will change only if handle_state returns TRUE.
    state <- values$state
    if (state %in% final_states) {
      # Go (rather than next) was pressed.
      shinyjs::show("stop")
      shinyjs::disable("go")
      shinyjs::hide("prev")
      async_data$epi_estim_output <- NULL # Remove current plot
    }
    values$error <- NULL
    session$sendCustomMessage(type="resetErrorBoxes", "") #nolint
    values$status <- "Processing..."
    tryCatch({
      switch(state,
             "1.1" = {
               TRUE
             },
             "2.1" = {
               # Handle uploaded data:
               if (is.null(input$incidence_data$datapath)) {
                 throw_error("Please upload a file", "incidence_data")
               }
               if (tools::file_ext(input$incidence_data$name) != "csv") {
                 throw_error("The uploaded file must be a .csv file",
                             "incidence_data")
               }
               incidence_data <<- read.csv(input$incidence_data$datapath,
                                           header = input$incidence_header,
                                           sep = ",",
                                           quote = "")
               # Process Incidence data (see utils.R)
               incidence_data <<- EpiEstim:::process_I(incidence_data)
               length <- dim(incidence_data)[1]
               w <- input$uploaded_width
               if (w >= length) {
                 throw_error(
                   paste("The width must be smaller than the length of your",
                         "incidence data"),
                   "uploaded_width")
                 throw_error(
                   paste("The width must be smaller than the length of your",
                         "incidence data"),
                   "incidecnce_data")
               }
               config$t_start <<- 2:(length - w + 1)
               config$t_end <<- (1 + w):length

               config$mean_prior <<- input$uploaded_mean_prior
               if (config$mean_prior < 0) {
                 throw_error("Prior mean must be non-negative",
                             "uploaded_mean_prior")
               }
               config$std_prior <<- input$uploaded_std_prior
               if (config$std_prior <= 0) {
                 throw_error("Prior standard deviation must be positive",
                             "uploaded_std_prior")
               }
               TRUE
             },
             "2.2" = {
               # Get preloaded data
               incidence_data <<-
                 get_incidence_data(input$incidence_dataset) #nolint

               # Process Incidence data (using EpiEstim)
               incidence_data <<- EpiEstim:::process_I(incidence_data) #nolint

               length <- dim(incidence_data)[1]
               w <- input$incidence_width
               if (w >= length) {
                 throw_error(
                   paste("The width must be smaller than the length of your",
                         "incidence data", "incidence_width"),
                   FALSE) # Don't stop until next one
                 throw_error(
                   paste("The width must be smaller than the length of your",
                         "incidence data"),
                   "incidence_data")
               }
               config$t_start <<- 2:(length - w + 1)
               config$t_end <<- (1 + w):length

               config$mean_prior <<- input$incidence_mean_prior
               if (config$mean_prior < 0) {
                 throw_error("Prior mean must be non-negative",
                             "incidence_mean_prior")
               }
               config$std_prior <<- input$incidence_std_prior
               if (config$std_prior <= 0) {
                 throw_error("Prior standard deviation must be positive",
                             "incidence_std_prior")
               }
               TRUE
             },
             "3.1" = {TRUE}, #nolint
             "4.1" = {
               incidence_data <<- read.csv(input$incidence_data$datapath,
                                          header = input$incidence_header,
                                          sep = ",",
                                          quote = ",")
               if (is.null(input$imported_data$datapath)) {
                 throw_error("Please upload a file", "imported_data")
               }
               if (tools::file_ext(input$imported_data$name) != "csv") {
                 throw_error("The uploaded file must be a .csv file",
                             "imported_data")
               }
               imported_data <- read.csv(input$imported_data$datapath,
                                        header = input$imported_header,
                                        sep = input$imported_sep,
                                        quote = input$imported_quote)
               if (dim(imported_data)[1] != dim(incidence_data)[1]) {
                 # Lengths don't match
                 stop(paste("The 'all cases' and the 'imported' datasets are",
                            "not of the same length"))
               }
               incidence_data$imported <<- imported_data[, 1]
               colnames(incidence_data) <<- c("local", "imported")

               # Currently the "local" column will be the total number of cases
               # because of the way the app is asking for inputs. Correct this.
               incidence_data$local <<- incidence_data$local -
                 incidence_data$imported

               # Process Incidence data (using EpiEstim)
               incidence_data <<- EpiEstim:::process_I(incidence_data)
               TRUE
             },
             "5.1" = {TRUE}, #nolint
             "6.1" = {TRUE}, #nolint
             "6.2" = {TRUE}, #nolint
             "7.1" = {TRUE}, #nolint
             "7.2" = {TRUE}, #nolint
             "7.3" = {
               method <<- "uncertain_si"
               config$n1 <<- input$n1
               config$n2 <<- input$n2
               config$mean_si <<- input$mean_si
               config$std_si <<- input$std_si
               config$std_mean_si <<- input$std_mean_si
               config$min_mean_si <<- input$min_mean_si
               config$max_mean_si <<- input$max_mean_si
               config$std_std_si <<- input$std_std_si
               config$min_std_si <<- input$min_std_si
               config$max_std_si <<- input$max_std_si
               if (is.null(config$n1) || is.na(config$n1) || config$n1 < 1 ||
                   !is.integer(config$n1)) {
                 throw_error("n1 must be an integer greater than or equal to 1",
                             "n1")
               }
               if (is.null(config$n2) || is.na(config$n2) || config$n2 < 1 ||
                   !is.integer(config$n2)) {
                 throw_error("n2 must be an integer greater than or equal to 1",
                             "n2")
               }
               if (is.null(config$mean_si) || is.na(config$mean_si) ||
                   config$ mean_si < 1) {
                 throw_error("mean_si must be greater than or equal to 1",
                             "mean_si")
               }
               if (is.null(config$min_mean_si) || is.na(config$min_mean_si) ||
                   config$min_mean_si < 1) {
                 throw_error("min_mean_si must be greater than or equal to 1",
                             "min_mean_si")
               }
               if (is.null(config$max_mean_si) || is.na(config$max_mean_si) ||
                   config$max_mean_si < 1) {
                 throw_error("max_mean_si must be greater than or equal to 1",
                             "max_mean_si")
               }
               if (is.null(config$std_mean_si) || is.na(config$std_mean_si) ||
                   config$std_mean_si <= 0) {
                 throw_error("std_mean_si must be greater than 0",
                             "std_mean_si")
               }
               if (config$min_mean_si > config$mean_si) {
                 throw_error("min_mean_si must be less than mean_si",
                             "min_mean_si", FALSE) # Don't stop until next one
                 throw_error("min_mean_si must be less than mean_si", "mean_si")
               }
               if (config$mean_si > config$max_mean_si) {
                 throw_error("max_mean_si must be greater than mean_si",
                             "max_mean_si", FALSE) # Don't stop until next one
                 throw_error("max_mean_si must be greater than mean_si",
                             "mean_si")
               }
               if (is.null(config$std_si) || is.na(config$std_si) ||
                   config$std_si <= 0) {
                 throw_error("std_si must be greater than 0", "std_si")
               }
               if (is.null(config$min_std_si) || is.na(config$min_std_si) ||
                   config$min_std_si <= 0) {
                 throw_error("min_std_si must be greater than 0", "min_std_si")
               }
               if (is.null(config$max_std_si) || is.na(config$max_std_si) ||
                   config$max_std_si <= 0) {
                 throw_error("max_std_si must be greater than 0", "max_std_si")
               }
               if (is.null(config$std_std_si) || is.na(config$std_std_si) ||
                   config$std_std_si <= 0) {
                 throw_error("std_std_si must be greater than 0", "std_std_si")
               }
               if (config$min_std_si > config$std_si) {
                 throw_error("min_std_si must be less than std_si",
                             "min_std_si", FALSE) # Don't stop until next one
                 throw_error("min_std_si must be less than std_si", "std_si")
               }
               if (config$std_si > config$max_std_si) {
                 throw_error("max_std_si must be greater than std_si",
                             "max_std_si", FALSE) # Don't stop until next one
                 throw_error("max_std_si must be greater than std_si", "std_si")
               }

               requested_seed <<- input$uncertain_seed
               if (!is.null(requested_seed) & !is.na(requested_seed)) {
                   # Actually set the seed now, to check it's valid
                   tryCatch({
                       set.seed(requested_seed)
                   },
                   error = function(e) {
                       throw_error("Invalid seed", "uncertain_seed")
                   })
               }
               TRUE
             },
             "7.4" = {
               config$mean_si <<- input$mean_si2
               config$std_si <<- input$std_si2
               method <<- "parametric_si"
               if (is.null(config$mean_si) || config$mean_si <= 1) {
                 throw_error("mean_si must be greater than 1", "mean_si2")
               }
               if (is.null(config$std_si) || config$std_si <= 0) {
                 throw_error("std_si must be greater than 0", "std_si2")
               }
               TRUE
             },
             "7.5" = {
               method <<- "non_parametric_si"
               if (is.null(input$si_distr_data$datapath)) {
                 throw_error("Please upload a file", "si_distr_data")
               }
               if (tools::file_ext(input$si_distr_data$name) != "csv") {
                 throw_error("The uploaded file must be a .csv file",
                             "si_distr_data")
               }
               config$si_distr <<- as.numeric(
                 read.csv(input$si_distr_data$datapath,
                          header = input$si_distr_header, sep = ",",
                          quote = "")
                 )
               TRUE
             },
             "7.6" = {
               method <<- "non_parametric_si"
               config$si_distr <<- get_si_distribution(input$si_distr_dataset) #nolint
               TRUE
             },
             "8.1" = {
               # "si_from_sample"
               # Simply read the MCMC samples from the file. See getMCMCFit in
               # utils.R
               method <<- "si_from_sample"
               si_sample <<- get_si_samples(input$si_dataset, input$si_dist) #nolint

               requested_seed <<- input$preloaded_seed
               if (!is.null(requested_seed) & !is.na(requested_seed)) {
                   # Actually set the seed now, to check it's valid
                   tryCatch({
                       set.seed(requested_seed)
                   },
                   error = function(e) {
                       throw_error("Invalid seed", "preloaded_seed")
                   })
               }

               config$n2 <<- input$n24
               if (is.null(config$n2) || config$n2 < 1 ||
                   !is.integer(config$n2)) {
                 throw_error("n2 must be an integer greater than or equal to 1",
                             "n24")
               }
               TRUE
             },
             "8.2" = {
               method <<- "si_from_data"
               if (is.null(input$si_data$datapath)) {
                 throw_error("Please upload a file", "si_data")
               }
               if (tools::file_ext(input$si_data$name) != "csv") {
                 throw_error("The uploaded file must be a .csv file", "si_data")
               }
               serial_interval_data <- read.csv(input$si_data$datapath,
                                              header = input$si_header,
                                              sep = ",",
                                              quote = "")
               # Process the data (see function in utils.R)
               si_data <<- EpiEstim:::process_si_data(serial_interval_data)
               requested_seed <<- input$uploaded_si_seed
               if (!is.null(requested_seed) & !is.na(requested_seed)) {
                   # Actually set the seed now, to check it's valid
                   tryCatch({
                       set.seed(requested_seed)
                   },
                   error = function(e) {
                       throw_error("Invalid seed", "uploaded_si_seed")
                   })

               }
               # Throw a warning about MCMC locking up app if only 1 core
               if (future::availableCores() == 1) {
                 shinyjs::alert(paste("WARNING:\n", "Your machine only has 1 core",
                             "available for EpiEstimApp to use. This means",
                             "that we cannot run MCMC in a separate process",
                             "which will cause the app to lock up while you",
                             "run MCMC. You may still run MCMC, however the",
                             "app will become completely unresponsive while",
                             "MCMC is running, which may take quite some time.",
                             sep = ""))
               }
               TRUE
             },
             "8.3" = {
               method <<- "si_from_sample"
               si_sample <<- EpiEstim:::process_si_sample(
                 read.csv(input$si_sample_data$datapath,
                          header = input$si_sample_header,
                          sep = ",",
                          quote = "")
                 )
               config$n2 <<- input$n23
               if (is.null(config$n2) || config$n2 < 1 ||
                   !is.integer(config$n2)) {
                 throw_error("n2 must be an integer greater than or equal to 1",
                             "n23")
               }
               requested_seed <<- input$si_sample_seed
               if (!is.null(requested_seed) & !is.na(requested_seed)) {
                   # Actually set the seed now, to check it's valid
                   tryCatch({
                       set.seed(requested_seed)
                   },
                   error = function(e) {
                       throw_error("Invalid seed", "si_sample_seed")
                   })
               }
               TRUE
             },
             "9.1" = {
               config$mcmc_control$burnin <<- input$burnin
               total_samples_needed <<- input$burnin + input$n12 * input$thin
               config$n1 <<- input$n12
               config$n2 <<- input$n22
               config$mcmc_control$thin <<- input$thin
               config$si_parametric_distr <<- input$si_dist_2
               mcmc_samples <<- async_data$mcmc_samples
               si_sample_from_data <<- async_data$si_sample_from_data
               convergence_check <<- async_data$convergence_check
               if (!is.na(input$param1) && !is.na(input$param1)) {
                 config$mcmc_control$init_pars <<- c(input$param1, input$param2)
               } else {
                 config$mcmc_control$init_pars <<- EpiEstim::init_mcmc_params(
                                                    si_data,
                                                    config$si_parametric_distr)
               }

               if (is.null(config$n1) || config$n1 < 1 ||
                   !is.integer(config$n1)) {
                 throw_error("n1 must be an integer greater than or equal to 1",
                             "n12")
               }
               if (is.null(config$n2) || config$n2 < 1 ||
                   !is.integer(config$n2)) {
                 throw_error("n2 must be an integer greater than or equal to 1",
                             "n22")
               }
               if (is.null(config$mcmc_control$thin) ||
                   config$mcmc_control$thin < 1 ||
                   !is.integer(config$mcmc_control$thin)) {
                 throw_error(paste("thin must be an integer greater than or",
                                   "equal to 1"), "thin")
               }
               if (is.null(config$mcmc_control$burnin) ||
                   config$mcmc_control$burnin < 0 ||
                   !is.integer(config$mcmc_control$burnin)) {
                 throw_error("burnin must be a non-negative integer", "burnin")
               }

              requested_mcmc_seed <<- input$mcmc_seed
               if (!is.null(requested_mcmc_seed) &
                   !is.na(requested_mcmc_seed)) {
                   # Actually set the seed now, to check it's valid
                   tryCatch({
                       set.seed(requested_mcmc_seed)
                   },
                   error = function(e) {
                       throw_error("Invalid seed", "mcmc_seed")
                   })
               }
               # MCMC LIMITING:
               # We don't want too many of our cores invested in running MCMC.
               # If all cores are in use, the app will lock up for ALL USERS.
               # To deal with this, we will limit the number of cores used for
               # MCMC.
               cores <- future::availableCores()
               current_mcmc <- length(
                 list.files(path = mcmc_pid_folder, pattern="*pid.txt"))
               if (current_mcmc >= ceiling(cores / 2)) {
                 shinyjs::alert(paste("ERROR: SERVER BUSY\n",
                      "Unfortunately the maximum number of MCMC processes are",
                      "already running on this server. This is probably",
                      "because other users are also running MCMC. Please try",
                      " again later.\n",
                      "Alternatively, please install EpiEstimApp on your",
                      "computer and run your own instance.", sep = ""))
                 values$status <- "SERVER BUSY"
                 shinyjs::hide("stop")
                 shinyjs::enable("go")
                 shinyjs::show("prev")
                 shinyjs::enable("prev")
                 FALSE
               } else {
                 TRUE
               }
             },
             stop(sprintf(paste("An error occurred in handle_state().",
                                "_input '%s' was not recognised."), state))
      )
    },
    error = function (e) {
      handle_error(values$state, e)
      FALSE
    })
  } #  End handle_state


  # get_next_state and get_prev_state encode the logic in the decision tree.
  # See Decision Tree_Schematic.pdf in the root of this project.
  get_next_state <- function (current_state) {
    switch(current_state,
           "1.1" = {
             if (input$incidence_data_type == "own") "2.1" else "2.2"
             },
           "2.1" = {"3.1"}, #nolint
           "2.2" = {"5.1"}, #nolint
           "3.1" = {if (input$imported == "TRUE") "4.1" else "5.1"},  #nolint
           "4.1" = {"5.1"}, #nolint
           "5.1" = {
             if (input$si_patient_data == "TRUE") "6.1" else "6.2"
             },
           "6.1" = {
             if (input$si_data_type == "preloaded") "7.1" else "7.2"
             },
           "6.2" = {
             if (input$si_est_type == "uncertain") {
               "7.3"
             } else if (input$si_est_type == "parametric") {
               "7.4"
             } else if (input$si_est_type == "own") {
               "7.5"
             } else if (input$si_est_type == "preloaded") {
               "7.6"
             } else {
               stop("Error in get_next_state(), si_est_type not found")
             }
           },
           "7.1" = {"8.1"}, #nolint
           "7.2" = {if (input$si_from == "data") "8.2" else "8.3"}, #nolint
           "8.2" = {"9.1"}, #nolint
           stop(sprintf(paste("An error occurred in get_next_state(). _input",
                              "'%s' was not recognised."), current_state))
    )
  }

  get_prev_state <- function (current_state) {
    switch(current_state,
           "2.1" = {"1.1"}, #nolint
           "2.2" = {"1.1"}, #nolint
           "3.1" = {"2.1"}, #nolint
           "4.1" = {"3.1"}, #nolint
           "5.1" = {
             if (input$incidence_data_type == "own") {
               if (input$imported == "TRUE") "4.1" else "3.1"
             } else {
               "2.2"
             }
           },
           "6.1" = {"5.1"}, #nolint
           "6.2" = {"5.1"}, #nolint
           "7.1" = {"6.1"}, #nolint
           "7.2" = {"6.1"}, #nolint
           "7.3" = {"6.2"}, #nolint
           "7.4" = {"6.2"}, #nolint
           "7.5" = {"6.2"}, #nolint
           "7.6" = {"6.2"}, #nolint
           "8.1" = {"7.1"}, #nolint
           "8.2" = {"7.2"}, #nolint
           "8.3" = {"7.2"}, #nolint
           "9.1" = {"8.2"}, #nolint
           stop(sprintf(paste("An error occurred in get_prev_state().",
                              "_input '%s' was not recognised."), current_state))
    )
  }

  ### The following is to make everything as asyncronous as possible to prevent
  ### slow functions being blocking.
  start_async_data_load <- function(async_data_name, future_obj) {
    check_async_data_being_loaded$suspend()
    async_data_being_loaded[[async_data_name]] <<- future_obj
    check_async_data_being_loaded$resume()
  } #end start_async_data_load

  check_async_data_being_loaded <- observe({
    shiny::invalidateLater(1000)
    for (async_data_name in names(async_data_being_loaded)) {
      async_future_object <- async_data_being_loaded[[async_data_name]]
      if (future::resolved(async_future_object)) {
        tryCatch({
          async_data[[async_data_name]] <<- future::value(async_future_object)
          async_data_being_loaded[[async_data_name]] <<- NULL

          # If we've resolved something but async_data$epi_estim_output is not
          # loaded then we've been incrementally running MCMC and are not done
          # yet. We want to re-start stuff, so call run() again
          if (is.null(async_data$epi_estim_output)) {
            run()
          }
        },
        error = function (e) {
          # Stop running, otherwise we'll throw the error every 1000ms.
          check_async_data_being_loaded$suspend()
          handle_error(values$state, e)
          FALSE
        })
      }
    }#end loop over async data items being loaded
    #if there are no more asynchronous data items being loaded, stop checking
    if (length(async_data_being_loaded) == 0) {
      check_async_data_being_loaded$suspend()
    }

    # If MCMC is being run, we should check on progress.
    if (method == "si_from_data") {
      prog <- get_mcmc_progress(progress_file) #nolint
      if (prog > 0 & prog < total_samples_needed) {
        values$status <- paste("Running MCMC (",
                               floor(100 * prog / total_samples_needed), "%)",
                               sep = "")
      }
    }
  }
  , suspended = TRUE) # check_async_data_being_loaded

  handle_error <- function(state, error) {
    # Add stop(error) in dev for detailed stack trace etc
    if (error$message == "handled") {
      # We've properly handled an error, and have used `stop("handled")` to
      # stop the app. Nothing should be done here in this case.
      return()
    }
    values$status <- "ERROR"
    cat("There was an error in state", state, "\n")
    cat(error$message, "\n")
    shinyjs::enable("go")
    shinyjs::hide("stop")
    shinyjs::show("prev")
    switch(state,
           "2.1" = {
             if (error$message ==
                 "incid must contain only non negative integer values."
                 ) {
               throw_error(
                 "Incidence data must contain only non negative integer values."
                 , "incidence_data", FALSE)
             } else if (error$message ==
                        paste("incid must be a vector or a dataframe with either",
                              "i) a column called 'I', or",
                              "ii) 2 columns called 'local' and 'imported'.")) {
               throw_error(paste("Incidence data must only contain one column,",
                                 "called 'I' or 'local'"), "incidence_data",
                           FALSE)
             } else if (error$message ==
                        "'file' must be a character string or connection") {
               throw_error("Please upload a valid csv file", "incidence_data",
                           FALSE)
             } else {
               shinyjs::info(error$message)
             }
           },
           "4.1" = {
             if (error$message ==
                 "'file' must be a character string or connection") {
               throw_error("Please upload a valid csvfile", "imported_data",
                           FALSE)
             } else {
               shinyjs::info(error$message)
             }
           },
           "7.5" = {
             file_processing_errors <- c(
               "si_distr must be a vector.",
               "si_distr should be so that si_distr[1] = 0.",
               "si_distr must be a positive vector.",
               "si_distr must sum to 1."
             )
             if (error$message ==
                 "'file' must be a character string or connection") {
               throw_error("Please upload a valid csv file", "si_distr_data",
                           FALSE)
             } else if (error$message %in% file_processing_errors) {
               throw_error(error$message, si_distr_data, FALSE)
             } else {
               shinyjs::info(error$message)
             }
           },
           "8.2" = {
             file_processing_errors <- c(
               paste("si_data has entries for which EL, ER, SL or SR",
                     "are non integers."),
               "si_data has entries for which ER<EL.",
               "si_data has entries for which SR<SL.",
               paste("You cannot fit any of the supported distributions to",
                     "this SI dataset, because for some data points the",
                     "maximum serial interval is <=0.")
             )
             if (error$message ==
                 "'file' must be a character string or connection") {
               throw_error("Please upload a valid csv file", "si_data", FALSE)
             } else if (error$message %in% file_processing_errors) {
               throw_error(error$message, "si_data", FALSE)
             } else {
               shinyjs::info(error$message)
             }
           },
           "8.3" = {
             file_processing_errors <- c(
               paste("method si_from_sample requires that si_sample[1,]",
                     "contains only 0."),
               paste("method si_from_sample requires that si_sample must",
                     "contain only non negtaive values."),
               paste("method si_from_sample requires the sum of each column",
                     "in si_sample to be 1.")
             )
             if (error$message ==
                 "'file' must be a character string or connection") {
               throw_error("Please upload a valid csv file", "si_sample_data",
                           FALSE)
             } else if (error$message %in% file_processing_errors) {
               throw_error(error$message, "si_sample_data", FALSE)
             } else {
               shinyjs::info(error$message)
             }
           },
           shinyjs::info(error$message) # Fallback to JS alert
    )
    return()
  }

  throw_error <- function(error_message, error_box_name = NULL, error=TRUE) {
    # Throws an error nicely. If you want to highlight a specific input in red,
    # give the id of that input (found in ui.R) as error_box_name. The
    # error_message will be displayed as some red text.
    # The error argument should be set to false only by the handle_error
    # function above. Otherwise it should be set to true to stop execution
    # (which will be happening inside a tryCatch)
    if (!is.null(error_box_name)) {
      session$sendCustomMessage(type = "error_box", error_box_name) #nolint
    }
    values$error <- error_message
    values$status <- "ERROR"
    shinyjs::enable("go")
    shinyjs::hide("stop")
    shinyjs::show("prev")
    # Throw an error to actually stop the app, but say we've handled telling
    # the client about the problem.
    if (error) {
      stop("handled")
    }
  }

  session$onSessionEnded(function() { #nolint
    check_async_data_being_loaded$suspend()
    if (file.exists(pid_file)) {
      # Something async is running (on unix), kill it.
      pid <- read.csv(pid_file, header = FALSE)
      tools::pskill(pid)
      file.remove(pid_file)
    }
    if (file.exists(progress_file)) {
      file.remove(progress_file)
    }
  })

  shiny::observeEvent(input$stop, {
    check_async_data_being_loaded$suspend()
    if (file.exists(pid_file)) {
      # Something async is running (on unix), kill it.
      pid <- read.csv(pid_file, header = FALSE)
      tools::pskill(pid)
      file.remove(pid_file)
    }
    if (file.exists(progress_file)) {
      file.remove(progress_file)
    }
    shinyjs::hide("stop")
    shinyjs::enable("go")
    shinyjs::show("prev")
    values$status <- "Ready"
  })

  observe({
    # This function removes the async_data$mcmc_samples whenever the
    # corresponding inputs change.
    input$si_data
    input$si_sep
    input$si_header
    input$si_quote
    input$si_dist2
    input$param1
    input$param2
    input$n12
    input$n22
    input$burnin
    input$thin

    async_data$mcmc_samples <<- NULL
    mcmc_samples <<- NULL
    async_data$si_sample_from_data <<- NULL
    si_sample_from_data <<- NULL
    async_data$convergence_check <<- NULL
    convergence_check <<- NULL
  })

}) # End shinyServer

