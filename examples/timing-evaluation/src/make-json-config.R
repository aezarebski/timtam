#' The configuration of the timing evaluation example.
#'
#'  - =acDuration= is the duration of the simulated data set
#'  - =acNumSims= is the number of simulations to attempt
#'  - =acBinWidth= is the desired spacing between the sizes of the simulations
#'  - =acNumBins= is the number of different sizes to simulate
#'  - =acSimParams= is the actual simulation parameters
#'  - =acOutputCsv= is where the application should store the results
#'  - =pyNumReplicates= is the number of times to average python evaluation ove
#'
library(magrittr)
library(jsonlite)

read_parameters <- function(duration, rho_prob) {
  example_params_json <- "../example-parameters.json"
  if (file.exists(example_params_json)) {
    example_params_list <- read_json(example_params_json)
    birth_rate <- example_params_list$birthRate
    death_rate <- example_params_list$deathRate
    sampling_rate <- example_params_list$samplingRate
    occurrence_rate <- example_params_list$occurrenceRate
    rm(example_params_list, example_params_json)
    list(
      mpParameters = list(
        birth_rate,
        death_rate,
        sampling_rate,
        list(c(duration - 1e-6, rho_prob)),
        occurrence_rate,
        list()
      ),
      mpDuration = duration
    )
  } else {
    stop("Cannot find JSON with example parameters!!!")
  }
}


main <- function(args) {
  if (dir.exists("out")) {
    duration <- 6.0
    rho_prob <- 0.5

    app_config <- list(
      acDuration = duration,
      acNumSims = 1000,
      acBinWidth = 5,
      acSimParams = read_parameters(duration, rho_prob),
      acNumBins = 40,
      acOutputCsv = "out/simulation-sizes-and-llhds.csv",
      pyNumReplicates = 10
    )
    write_json(
      x = app_config,
      path = "app-config.json",
      pretty = TRUE,
      auto_unbox = TRUE,
      digits = 7
    )
  } else {
    err_message <- "\n\n---> cannot find the output directory: out <---\n\n"
    stop(err_message)
  }
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}
