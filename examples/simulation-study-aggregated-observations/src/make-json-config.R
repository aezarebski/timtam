library(purrr)
library(magrittr)

if (not(dir.exists("out"))) {
  err_message <- "\n\n---> cannot find the output directory: out <---\n\n"
  stop(err_message)
}

output_file <- "agg-app-config.json"


simulation_duration <- 17



birth_rate <- 1.5
death_rate <- 0.50
sampling_rate <- 0.2
occurrence_rate <- 0.2

disaster_times <- seq(from = 2, to = simulation_duration, by = 1.5)
num_disasters <- length(disaster_times)
disaster_probs <- rep(0.15, num_disasters)
disaster_params <- map2(disaster_times, disaster_probs, list)

catastrophe_times <- disaster_times + 0.5
num_catastrophes <- length(catastrophe_times)
catastrophe_probs <- seq(from = 0.15, to = 0.25, length = num_catastrophes)
catastrophe_params <- map2(catastrophe_times, catastrophe_probs, list)

inference_configuration <- function(inf_time) {
    list(inferenceTime = inf_time,
         reconstructedTreeOutputFiles = sprintf(c("out/reconstructed-newick-tree-%.2f.txt",
                                                  "out/reconstructed-newick-metadata-%.2f.csv"),
                                                inf_time),
         observationsOutputCsv = sprintf("out/simulated-observations-%.2f.csv",
                                         inf_time),
         llhdOutputCsv = sprintf("out/llhd-evaluations-%.2f.csv",
                                 inf_time),
         pointEstimatesCsv = sprintf("out/final-negative-binomial-%.2f.csv",
                                     inf_time),
         aggregateObservations = FALSE # TODO Fix this so there is one true and once false.
         )
}


sim_params <- list(birth_rate, death_rate, sampling_rate, catastrophe_params, occurrence_rate, disaster_params)


result <- list(
    simulatedEventsOutputCsv = "out/all-simulated-events.csv",
    simulationParameters = sim_params,
    simulationDuration = simulation_duration + 1e-5,
    simulationSizeBounds = c(100,100000),
    inferenceConfigurations = list(inference_configuration(5),
                                   inference_configuration(5))
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
