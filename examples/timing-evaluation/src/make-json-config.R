library(purrr)
library(magrittr)
library(jsonlite)

if (not(dir.exists("out"))) {
  err_message <- "\n\n---> cannot find the output directory: out <---\n\n"
  stop(err_message)
}

output_file <- "app-config.json"

## the example parameters are expressed in units of days so this is 15 days.
simulation_duration <- 30

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

## scheduled the catastrophe for just prior to the end of the simulation to
## ensure it is processed. Since popsize-distribution does not support disasters
## we must leave this list empty.
catastrophe_params <- list(list(simulation_duration - 1e-10, 0.5))
disaster_params <- list()

sim_params <- list(
  birth_rate,
  death_rate,
  sampling_rate,
  catastrophe_params,
  occurrence_rate,
  disaster_params
)


result <- list(
  acDuration = simulation_duration,
  acNumSims = 50000,
  acBinWidth = 10,
  acSimParams = list(
    mpParameters = sim_params,
    mpDuration = simulation_duration
  ),
  acNumBins = 50,
  acOutputCsv = "out/simulation-sizes-and-llhds.csv",
  pyNumReplicates = 10
)

write_json(result,
  output_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = 16
)
