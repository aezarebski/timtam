library(purrr)
library(magrittr)
library(dplyr)

if (interactive()) {
    config_file <- "config.json"
} else {
    args <- commandArgs(trailingOnly = TRUE)
    config_file <- head(args, 1)
    if (!file.exists(config_file)) {
        stop(sprintf("Cannot find configuration file: %s\n", config_file))
    }
}



config <- jsonlite::read_json(config_file)

raw_simulation_data <- jsonlite::read_json(config$simulationJson)

simulation_data <- raw_simulation_data %>%
    map(~ data.frame(delay = .x[[1]], event = .x[[2]]$tag)) %>%
    bind_rows %>%
    mutate(time = cumsum(delay), reverse_time = config$simulationDuration - time)

event_types <- unique(simulation_data$event)

event_times_list <- map(event_types, ~ filter(simulation_data, event == .x)$reverse_time) %>% set_names(event_types)

jsonlite::write_json(x = event_times_list, path = config$popsizeDistributionFile, pretty = TRUE)
