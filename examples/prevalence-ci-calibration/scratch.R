library(purrr)
library(magrittr)
library(jsonlite)

if (not(dir.exists("out"))) {
  stop("Output directory does not exist!!!")
}

num_mcmc_samples <- 5e4 # the number of MCMC iterations to use.
simulation_duration <- 13.5




birth_rate <- 1.7
death_rate <- 0.9
sampling_rate <- 0.05
occurrence_rate <- 0.25

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


sim_seed <- 1

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
  simulationSizeBounds = c(3000, 7000),
  inferenceConfigurations = list(
    inference_configuration("true-params-regular-data", NULL, NULL, output_dir),
    inference_configuration(
      "est-params-regular-data",
      NULL,
      mcmc_configuration(
        "regular-data-mcmc-samples.csv",
        num_mcmc_samples,
        1e-2,
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
      NULL,
      output_dir
    )
  ),
  isVerbose = TRUE,
  configSimulationSeed = sim_seed
)

write_json(result,
  config_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = 7
)
