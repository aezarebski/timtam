library(purrr)
library(jsonlite)

output_file <- "ts-config.json"


simulation_duration <- 17

birth_rate <- 1.7
death_rate <- 0.30
sampling_rate <- 0.3
occurrence_rate <- 0.3

disaster_times <- seq(from = 2, to = simulation_duration, by = 1.5)
num_disasters <- length(disaster_times)
disaster_probs <- rep(0.25, num_disasters)
disaster_params <- map2(disaster_times, disaster_probs, list)


catastrophe_times <- disaster_times + 0.5
num_catastrophes <- length(catastrophe_times)
catastrophe_probs <- seq(from = 0.25, to = 0.25, length = num_catastrophes)
catastrophe_params <- map2(catastrophe_times, catastrophe_probs, list)

sim_params <- list(birth_rate, death_rate, sampling_rate, catastrophe_params, occurrence_rate, disaster_params)

num_eval_steps <- 100 # the number of mesh points for plotting
eval_lambda <- seq(from = 1.0, to = 3.0, length = num_eval_steps)
eval_params_lambda <- map(.x = eval_lambda, .f = ~ list(.x, death_rate, sampling_rate, catastrophe_params, occurrence_rate, disaster_params))
eval_mu <- seq(from = 0.05, to = 0.5, length = num_eval_steps)
eval_params_mu <- map(.x = eval_mu, .f = ~ list(birth_rate, .x, sampling_rate, catastrophe_params, occurrence_rate, disaster_params))
eval_params <- c(eval_params_lambda, eval_params_mu)


result <- list(
    simulatedEventsOutputCsv = "out/all-simulated-events.csv",
    simulationParameters = sim_params,
    simulationDuration = simulation_duration + 1e-5,
    reconstructedTreeOutputFiles = c("out/reconstructed-newick-tree.txt","out/reconstructed-newick-metadata.csv"),
    observationsOutputCsv = "out/simulated-observations.csv",
    evaluationParameters = eval_params,
    llhdOutputCsv = "out/llhd-evaluations.csv"
)

write_json(result, output_file, pretty = FALSE, auto_unbox = TRUE, digits = 7)

eval_df <- data.frame(parameter = rep(c("lambda", "mu"), each = num_eval_steps),
                      value = c(eval_lambda, eval_mu))

write.table(x = eval_df, file = "out/evaluation-parameters.csv", row.names = FALSE)

## save a copy of the true parameters so they can be read out later rather than
## hardcoded.
true_parameters <- data.frame(parameter = c("lambda", "mu"),
                              value = c(birth_rate,death_rate))
write.table(x = true_parameters,
            file = "out/true-parameters.csv",
            row.names = FALSE)
