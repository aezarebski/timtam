library(purrr)
library(magrittr)

if (not(dir.exists("out"))) {
  err_message <- "\n\n---> cannot find the output directory: out <---\n\n"
  stop(err_message)
}

output_file <- "agg-app-config.json"


simulation_duration <- 8.5 - 1e-6

birth_rate <- 1.7
death_rate <- 0.5
sampling_rate <- 0.2
occurrence_rate <- 0.3

disaster_params <- list()
catastrophe_params <- list()

## For the aggregation, these are the times at which we carry out the
## aggregation.
seq_agg_times <- as.list(seq(from = 2.5, to = 8.5, by = 1))
unseq_agg_times <- as.list(seq(from = 2.4, to = 8.4, by = 1))



#' Return a list corresponding to the \code{InferenceConfiguration} from
#' \code{Main.hs}.
#'
#' @param inf_config_name a character label for the inference
#' @param agg_times_vec a list of numeric vectors defining the aggregation
#'   times, first for the sequenced and then unsequenced samples, or \code{NULL}
#'   if there are no aggregation times.
#'
inference_configuration <- function(inf_config_name, agg_times_vec) {
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
  simulationDuration = simulation_duration + 1e-5,
  simulationSizeBounds = c(100, 100000),
  inferenceConfigurations = list(
    inference_configuration("true-params-regular-data", NULL),
    inference_configuration("est-params-regular-data", NULL),
    inference_configuration("est-params-agg-data",
                            list(seq_agg_times,
                                 unseq_agg_times))
  ),
  isVerbose = TRUE
)

jsonlite::write_json(result,
  output_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = 7
)
