library(jsonlite)
library(purrr)

OUTPUT_FILE <- "time-dependent-rates-config.json"

simulation_duration <- 5.0
birth_rate_change_time <- 3.0

birth_rate_1_true <- 3.0
birth_rate_2_true <- 1.0

birth_rate_1_vals <- seq(from = 0.1, to = 10, length = 50)
birth_rate_2_vals <- seq(from = 0.1, to = 10, length = 50)

death_rate <- 0.7

sampling_rate <- 0.7


config <- list(simulationParameters = list(list(c(0.0,birth_rate_1_true),c(birth_rate_change_time,birth_rate_2_true),c(1e10,1.0)),death_rate,sampling_rate),
               simulationDuration = simulation_duration,
               simulationEventsFile = "fake-foo.csv",
               inferenceParameters = c(map(birth_rate_1_vals, ~ list(list(c(0.0,.x),c(birth_rate_change_time,birth_rate_2_true),c(1e10,1.0)),death_rate,sampling_rate)),
                                       map(birth_rate_2_vals, ~ list(list(c(0.0,birth_rate_1_true),c(birth_rate_change_time,.x),c(1e10,1.0)),death_rate,sampling_rate))),
               inferenceLlhdFile = "fake-bar.csv")

write_json(x = config, path = OUTPUT_FILE, auto_unbox = TRUE, digits = 16)
