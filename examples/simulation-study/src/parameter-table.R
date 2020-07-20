library(whisker)
library(jsonlite)

template <- readLines("src/parameter-table-template.tex")

json_data <- read_json("out/config.json")


data <- list(birth_rate = json_data$simLambda,
             death_rate = json_data$simMu,
             sampling_rate = json_data$simPsi,
             catastrophe_prob = json_data$simRho,
             catastrophe_times = paste(sprintf("%.1f", json_data$simRhoTimes), collapse = ", "),
             disaster_prob = json_data$simNu,
             disaster_times = paste(sprintf("%.1f", json_data$simNuTimes), collapse = ", "),
             occurrence_rate = json_data$simOmega,
             simulation_duration = json_data$simDuration
             )

writeLines(whisker.render(template, data), "out/parameter-table.tex")
