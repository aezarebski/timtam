library(purrr)
library(jsonlite)

output_file <- "ts-config.json"


simulation_duration <- 17

## We want to see how the inference changes over times so we set several time
## points at which to generate estimates. This vector is used to specify when
## they occur.
inference_times <- seq(from = 7, to = 17, by = 4) 

## These are the values of the parameters used in the simulation, we put them
## here so they stand out and we can re-use them in subsequent parts of the
## specification.
birth_rate <- 1.5
death_rate <- 0.50
sampling_rate <- 0.2
catastrophe_prob <- NULL
occurrence_rate <- 0.2
disaster_prob <- 0.15


disaster_times <- seq(from = 2, to = simulation_duration, by = 1.5)
num_disasters <- length(disaster_times)
disaster_probs <- rep(disaster_prob, num_disasters)
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
                                     inf_time))
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
  simulationParametersClean = list(lambda = birth_rate,
                                   mu = death_rate,
                                   psi = sampling_rate,
                                   omega = occurrence_rate),
  simulationDuration = simulation_duration + 1e-5,
  simulationSizeBounds = c(100,100000),
  inferenceConfigurations = map(inference_times, inference_configuration),
  partialEvaluationOutputCsv = "out/partial-evaluations.csv"
)

write_json(result,
           output_file,
           pretty = TRUE,
           auto_unbox = TRUE,
           digits = 7)
