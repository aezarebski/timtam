library(jsonlite)
library(dplyr)
library(purrr)
library(magrittr)

#' Read in the input JSON and reformat the data so that it can be used by the
#' \code{popsize_distribution} code.
#'
#' @param input_json character filepath for the simulated data
#' @param output_json character filepath for the file to write to
#' @param simulation_duration numeric of the simulation duration so time can be
#'   calculated in reverse.
#'
reformat_observations <- function(input_json,
                                  output_json,
                                  simulation_duration) {
    simulation_data <- input_json %>% read_json %>%
        map(~ data.frame(delay = .x[[1]], event = .x[[2]]$tag)) %>%
        bind_rows %>%
        mutate(time = cumsum(delay),
               reverse_time = simulation_duration - time)

    event_types <- unique(simulation_data$event)

    event_times_list <- map(event_types,
                            ~ filter(simulation_data, event == .x)$reverse_time) %>%
        set_names(event_types)

    write_json(x = event_times_list,
               path = output_json,
               pretty = TRUE)
}


main <- function() {
  app_config <- read_json("app-config.json")
  SIMULATION_DURATION <- app_config$acDuration

  simulation_filepaths <- list.files(path = "out/",
                                     pattern = "^simulated",
                                     full.names = TRUE)

  output_filepaths <- gsub(pattern = "simulated",
                           replacement = "reformated-simulated",
                           simulation_filepaths)

  map2(simulation_filepaths,
       output_filepaths,
       reformat_observations,
       SIMULATION_DURATION)
}

if (!interactive()) {
  main()
}

