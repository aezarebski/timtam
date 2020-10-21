library(purrr)
library(jsonlite)

output_file <- "ts-config.json"


simulation_duration <- 17

## We want to see how the inference changes over times so we set several time
## points at which to generate estimates. This vector is used to specify when
## they occur.
inference_times <- seq(from = 12, to = 17, by = 4)

## These are the values of the parameters used in the simulation, we put them
## here so they stand out and we can re-use them in subsequent parts of the
## specification.
birth_rate <- 1.5
death_rate <- 0.50
sampling_rate <- 0.2
catastrophe_prob <- 0.2
occurrence_rate <- 0.2
disaster_prob <- 0.15


disaster_times <- seq(from = 2, to = simulation_duration, by = 1.5)
num_disasters <- length(disaster_times)
disaster_probs <- rep(disaster_prob, num_disasters)
disaster_params <- map2(disaster_times, disaster_probs, list)

catastrophe_times <- disaster_times + 0.5
num_catastrophes <- length(catastrophe_times)
catastrophe_probs <- rep(catastrophe_prob, num_catastrophes)
catastrophe_params <- map2(catastrophe_times, catastrophe_probs, list)

#' Return a list describing the inference based on a the data available at the
#' \code{inf_time}. This is applied to the inference times defined above.
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
inference_configurations <- map(inference_times,
                                inference_configuration)

sim_params <- list(birth_rate,
                   death_rate,
                   sampling_rate,
                   catastrophe_params,
                   occurrence_rate,
                   disaster_params)

## This is the specification of the mesh used in the evaluation of the
## likelihood profiles. The values indicate the range over which to vary the
## parameter and the mesh size refers to the number of points to evaluate the
## likelihood at for each profile.
llhd_profile_mesh <- list(
  lpmLambdaBounds = c(1.2, 1.9),
  lpmMuBounds = c(0.1, 0.9),
  lpmPsiBounds = c(0.10,0.3),
  lpmOmegaBounds = c(0.15,0.35),
  lpmMeshSize = 100,
  lpmRhoBounds = c(0.10, 0.3),
  lpmNuBounds = c(0.10, 0.2)
)


result <- list(
  simulatedEventsOutputCsv = "out/all-simulated-events.csv",
  simulationParameters = sim_params,
  simulationParametersClean = list(lambda = birth_rate,
                                   mu = death_rate,
                                   psi = sampling_rate,
                                   rho = catastrophe_prob,
                                   omega = occurrence_rate,
                                   nu = disaster_prob),
  simulationDuration = simulation_duration + 1e-6,
  simulationSizeBounds = c(100,100000),
  inferenceConfigurations = inference_configurations,
  partialEvaluationOutputCsv = "out/partial-evaluations.csv",
  acLlhdProfileMesh = llhd_profile_mesh
)

write_json(result,
           output_file,
           pretty = TRUE,
           auto_unbox = TRUE,
           digits = 7)
