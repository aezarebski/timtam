library(purrr)
library(magrittr)
library(jsonlite)

if (not(dir.exists("out"))) {
  err_message <- "\n\n---> cannot find the output directory: out <---\n\n"
  stop(err_message)
}

output_file <- "ts-config.json"


simulation_duration <- 30

## We want to see how the inference changes over times so we set several time
## points at which to generate estimates. This vector is used to specify when
## they occur.
inference_times <- seq(from = 25, to = simulation_duration, by = 1)

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
}

## In this example we also want to have some scheduled observations and these
## are not in the sared JSON so we define them here. Then we need to construct
## the list which specifies when these occur in a way that the executable
## understands.
sched_interval <- 4
catastrophe_prob <- 0.5 * example_params_list$samplingRate
disaster_prob <- 0.5 * example_params_list$occurrenceRate

disaster_times <- seq(from = sched_interval, to = simulation_duration, by = sched_interval)
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
  lpmLambdaBounds = c(0.1, 0.5),
  lpmMuBounds = c(0.01, 0.2),
  lpmPsiBounds = c(0.01, 0.1),
  lpmOmegaBounds = c(0.01, 0.1),
  lpmMeshSize = 100,
  lpmRhoBounds = c(0.01, 0.6),
  lpmNuBounds = c(0.01, 0.6)
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
  simulationDuration = simulation_duration,
  simulationSizeBounds = c(100,10000),
  simulationSeed = 100,
  inferenceConfigurations = inference_configurations,
  acLlhdProfileMesh = llhd_profile_mesh
)

write_json(result,
           output_file,
           pretty = TRUE,
           auto_unbox = TRUE,
           digits = 7)
