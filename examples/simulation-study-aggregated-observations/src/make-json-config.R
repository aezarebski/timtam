#' This script generates the configuration file for the
#' \code{simulation-study-aggregated-observations} application, the resulting
#' JSON needs to conform to the type of the \code{Configuration} object
#' described in the source code for that application.

library(purrr)
library(magrittr)
library(jsonlite)

if (not(dir.exists("out"))) {
  err_message <- "\n\n---> cannot find the output directory: out <---\n\n"
  stop(err_message)
}

output_file <- "agg-app-config.json"

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
#'
inference_configuration <- function(inf_config_name, agg_times_vec, mcmc_config) {
  result <- list(
    reconstructedTreeOutputFiles = sprintf(
      c(
        "out/reconstructed-newick-tree-%s.txt",
        "out/reconstructed-newick-metadata-%s.csv"
      ),
      inf_config_name
    ),
    observationsOutputCsv = sprintf(
      "out/simulated-observations-%s.csv",
      inf_config_name
    ),
    llhdOutputCsv = sprintf(
      "out/llhd-evaluations-%s.csv",
      inf_config_name
    ),
    pointEstimatesCsv = sprintf(
      "out/final-negative-binomial-%s.csv",
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


result <- list(
  simulatedEventsOutputCsv = "out/all-simulated-events.csv",
  simulationParameters = sim_params,
  simulationDuration = simulation_duration,
  simulationSizeBounds = c(1000, 10000),
  inferenceConfigurations = list(
    inference_configuration("true-params-regular-data", NULL, NULL),
    inference_configuration(
      "est-params-regular-data",
      NULL,
      mcmc_configuration(
        "regular-data-mcmc-samples.csv",
        num_mcmc_samples,
        1e-2,
        7 # the mcmc seed
      )
    ),
    inference_configuration(
      "est-params-agg-data",
      list(
        seq_agg_times,
        unseq_agg_times
      ),
      mcmc_configuration(
        "aggregated-data-mcmc-samples.csv",
        num_mcmc_samples,
        1e-2,
        7 # the mcmc seed
      )
    )
  ),
  isVerbose = TRUE,
  configSimulationSeed = 46
)

write_json(result,
  output_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = 7
)
