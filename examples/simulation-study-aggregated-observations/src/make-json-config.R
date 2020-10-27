library(purrr)
library(magrittr)

if (not(dir.exists("out"))) {
  err_message <- "\n\n---> cannot find the output directory: out <---\n\n"
  stop(err_message)
}

output_file <- "agg-app-config.json"


simulation_duration <- 3.5 - 1e-6



birth_rate <- 1.3
death_rate <- 0.50
sampling_rate <- 0.2
occurrence_rate <- 0.0

disaster_params <- list()

catastrophe_params <- list()


inference_configuration <- function(inf_config_name, agg_times_vec) {
    result <- list(
      reconstructedTreeOutputFiles = sprintf(c("out/reconstructed-newick-tree-%s.txt",
                                               "out/reconstructed-newick-metadata-%s.csv"),
                                             inf_config_name),
      observationsOutputCsv = sprintf("out/simulated-observations-%s.csv",
                                      inf_config_name),
      llhdOutputCsv = sprintf("out/llhd-evaluations-%s.csv",
                              inf_config_name),
      pointEstimatesCsv = sprintf("out/final-negative-binomial-%s.csv",
                                  inf_config_name)
    )
    if (not(is.null(agg_times_vec))) {
      result$icMaybeTimesForAgg <- agg_times_vec
    }
    return(result)
}


sim_params <- list(birth_rate,
                   death_rate,
                   sampling_rate,
                   catastrophe_params,
                   occurrence_rate,
                   disaster_params)


result <- list(
    simulatedEventsOutputCsv = "out/all-simulated-events.csv",
    simulationParameters = sim_params,
    simulationDuration = simulation_duration + 1e-5,
    simulationSizeBounds = c(100,100000),
    inferenceConfigurations = list(inference_configuration("true-params-regular-data", NULL),
                                   inference_configuration("est-params-regular-data", NULL),
                                   inference_configuration("est-params-agg-data", c(2.5,3.5))),
    isVerbose = TRUE
)

jsonlite::write_json(result,
                     output_file,
                     pretty = TRUE,
                     auto_unbox = TRUE,
                     digits = 7)



## save a copy of the true parameters so they can be read out later rather than
## hardcoded.
true_parameters <- data.frame(parameter = c("lambda", "mu"),
                              value = c(birth_rate,death_rate))
write.table(x = true_parameters,
            file = "out/true-parameters.csv",
            row.names = FALSE)