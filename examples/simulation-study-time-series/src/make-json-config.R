library(purrr)
library(jsonlite)

output_file <- "ts-config.json"


death_rate <- 0.8

disaster_params <- list()
catastrophe_params <- list(list(3.5,0.6),
                           list(4.5,0.3),
                           list(5.5,0.4),
                           list(6.5,0.3),
                           list(7.5,0.3))
sim_params <- list(2.3, death_rate, 0.0, catastrophe_params, 0.0, disaster_params)

eval_params <- map(.x = seq(from = 2.5, to = 6.5, length = 10), .f = ~ list(.x, death_rate, 0.0, catastrophe_params, 0.0, disaster_params))

result <- list(
    simulatedEventsOutputCsv = "out/all-simulated-events.csv",
    simulationParameters = sim_params, # [3.5, 0.1, 0.0, [[1.5,0.2],[2.4,0.3],[3.4,0.3]], 0.0, [[1.0,0.1]]],
    simulationDuration = 4.5 + 1e-5,
    reconstructedTreeOutputFiles = c("out/reconstructed-newick-tree.txt","out/reconstructed-newick-metadata.csv"),
    observationsOutputCsv = "out/simulated-observations.csv",
    evaluationParameters = eval_params,
    llhdOutputCsv = "out/llhd-evaluations.csv"
)

write_json(result, output_file, pretty = FALSE, auto_unbox = TRUE, digits = 7)
