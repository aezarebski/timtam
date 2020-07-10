library(jsonlite)
library(dplyr)
library(purrr)
library(magrittr)

SIMULATION_DURATION <- 10

simulation_filepaths <- list.files(path = "out/",
                                   pattern = "^simulated",
                                   full.names = TRUE)

output_filepaths <- gsub(pattern = "simulated",
                         replacement = "reformated-simulated",
                         simulation_filepaths)



## x <- simulation_filepaths[1]
## y <- output_filepaths[1]

reformat_observations <- function(x, y) {
    print("=====================")
    print(x)
    print(y)
    simulation_data <- x %>% read_json %>%
        map(~ data.frame(delay = .x[[1]], event = .x[[2]]$tag)) %>%
        bind_rows %>%
        mutate(time = cumsum(delay),
               reverse_time = SIMULATION_DURATION - time)

    event_types <- unique(simulation_data$event)

    event_times_list <- map(event_types, ~ filter(simulation_data, event == .x)$reverse_time) %>%
        set_names(event_types)

    jsonlite::write_json(x = event_times_list, path = y, pretty = TRUE)
    return(0)
}

map2(simulation_filepaths,
     output_filepaths,
     reformat_observations)
