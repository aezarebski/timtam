library(purrr)
library(magrittr)
library(jsonlite)

if (not(dir.exists("out"))) {
  stop("Output directory does not exist!!!")
}

num_mcmc_samples <- list(
  regular_data = 4e3,
  aggregated_data = 7e3
) # the number of MCMC iterations to use.
simulation_duration <- 9.5

## Read in the parameters to use in the example from a configuration file so
## they are shared between examples.
example_params_json <- "../example-parameters.json"
if (not(file.exists(example_params_json))) {
  stop("Cannot find JSON with example parameters!!!")
} else {
  example_params_list <- read_json(example_params_json)
  birth_rate <- example_params_list$birthRate
  death_rate <- example_params_list$deathRate
  sampling_rate <- example_params_list$samplingRate
  occurrence_rate <- example_params_list$occurrenceRate
  rm(example_params_list, example_params_json)
}

disaster_params <- list()
catastrophe_params <- list()

## For the aggregation, these are the times at which we carry out the
## aggregation.
time_mesh <- seq(from = 2.5, to = simulation_duration, by = 1)
seq_agg_times <- as.list(time_mesh)
unseq_agg_times <- as.list(time_mesh - 0.1)

#' Return a list corresponding to the \code{MCMCConfiguration} from \code{Main.hs}
#'
#' @param output_file a filepath for the mcmc samples
#' @param num_iters a numeric defining the number of iterations
#' @param step_sd a numeric defining the step standard deviation
#' @param seed a numeric defining the seed to use for the PRNG
#'
mcmc_configuration <- function(output_file, num_iters, step_sd, seed) {
  result <- list(
    mcmcOutputCSV = output_file,
    mcmcNumIters = num_iters,
    mcmcStepSD = step_sd,
    mcmcSeed = seed
  )
  return(result)
}


#' Return a list corresponding to the \code{InferenceConfiguration} from
#' \code{Main.hs}.
#'
#' @param inf_config_name a character label for the inference
#' @param agg_times_vec a list of numeric vectors defining the aggregation
#'   times, first for the sequenced and then unsequenced samples, or \code{NULL}
#'   if there are no aggregation times.
#' @param mcmc_config a list of MCMC configuration such as the one returned by
#'   \code{mcmc_configuration}.
#' @param output_dir a character label for the output directory
#'
inference_configuration <- function(inf_config_name, agg_times_vec, mcmc_config, output_dir) {
  result <- list(
    reconstructedTreeOutputFiles = sprintf(
      c(
        "%s/reconstructed-newick-tree-%s.txt",
        "%s/reconstructed-newick-metadata-%s.csv"
      ),
      output_dir,
      inf_config_name
    ),
    observationsOutputCsv = sprintf(
      "%s/simulated-observations-%s.csv",
      output_dir,
      inf_config_name
    ),
    llhdOutputCsv = sprintf(
      "%s/llhd-evaluations-%s.csv",
      output_dir,
      inf_config_name
    ),
    pointEstimatesCsv = sprintf(
      "%s/final-negative-binomial-%s.csv",
      output_dir,
      inf_config_name
    )
  )
  if (not(is.null(agg_times_vec))) {
    result$icMaybeTimesForAgg <- agg_times_vec
  }

  if (not(is.null(mcmc_config))) {
    result$icMaybeMCMCConfig <- mcmc_config
  }
  return(result)
}


sim_params <- list(
  birth_rate,
  death_rate,
  sampling_rate,
  catastrophe_params,
  occurrence_rate,
  disaster_params
)

## This is where we actually create the final configuration object and write it
## to file so we can iterate over difference seed values here.



make_config_file <- function(sim_seed) {
  output_dir <- sprintf("out/seed-%d", sim_seed)
  config_file <- sprintf("%s/config-%d.json", output_dir, sim_seed)

  if (not(dir.exists(output_dir))) {
    message("Making output directory: ", output_dir)
    dir.create(output_dir)
  }


  result <- list(
    simulatedEventsOutputCsv = sprintf("%s/all-simulated-events.csv", output_dir),
    simulationParameters = sim_params,
    simulationDuration = simulation_duration,
    simulationSizeBounds = c(1000, 10000),
    inferenceConfigurations = list(
      inference_configuration("true-params-regular-data", NULL, NULL, output_dir),
      inference_configuration(
        "est-params-regular-data",
        NULL,
        mcmc_configuration(
          sprintf("%s/regular-data-mcmc-samples.csv", output_dir),
          num_mcmc_samples$regular_data,
          5e-2,
          7 # the mcmc seed
        ),
        output_dir
      ),
      inference_configuration(
        "est-params-agg-data",
        list(
          seq_agg_times,
          unseq_agg_times
        ),
        mcmc_configuration(
          sprintf("%s/aggregated-data-mcmc-samples.csv", output_dir),
          num_mcmc_samples$aggregated_data,
          5e-2,
          7 # the mcmc seed
        ),
        output_dir
      )
    ),
    isVerbose = TRUE,
    configSimulationSeed = 100 * sim_seed
  )

  write_json(result,
    config_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = 7
  )
}





main <- function(args) {
  seed_start <- as.integer(args[1])
  seed_stop <- as.integer(args[2])

  if (all(c(is.integer(seed_stop),
            is.integer(seed_start),
            seed_start < seed_stop,
            seed_start > 0))) {
    for (sim_seed in seed_start:seed_stop) {
      make_config_file(sim_seed)
    }
  } else {
    stop("Could not get num_seeds from command line argument.")
  }
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}
