library(purrr)
library(magrittr)
library(jsonlite)

if (not(dir.exists("out"))) {
  err_message <- "\n\n---> cannot find the output directory: out <---\n\n"
  stop(err_message)
}

output_file <- "app-config.json"

simulation_duration <- 6

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

catastrophe_params <- list(list(5.999999, 0.5))
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
  acNumSims = 2000,
  acBinWidth = 5,
  acSimParams = list(
    mpParameters = sim_params,
    mpDuration = simulation_duration
  ),
  acNumBins = 40,
  acOutputCsv = "out/simulation-sizes-and-llhds.csv",
  pyNumReplicates = 50
)

write_json(result,
  output_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = 7
)
