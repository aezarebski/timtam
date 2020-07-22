library(purrr)
library(jsonlite)

output_file <- "ts-config.json"


simulation_duration <- 18

birth_rate <- 2.5
death_rate <- 0.70

disaster_times <- seq(from = 2, to = simulation_duration, by = 1)
num_disasters <- length(disaster_times)
disaster_probs <- rep(0.5, num_disasters)
disaster_params <- map2(disaster_times, disaster_probs, list)


catastrophe_times <- disaster_times + 0.5
num_catastrophes <- length(catastrophe_times)
catastrophe_probs <- seq(from = 0.5, to = 0.8, length = num_catastrophes)
catastrophe_params <- map2(catastrophe_times, catastrophe_probs, list)

sim_params <- list(birth_rate, death_rate, 0.0, catastrophe_params, 0.0, disaster_params)

num_eval_steps <- 100
eval_lambda <- seq(from = 1.5, to = 4.5, length = num_eval_steps)
eval_params_lambda <- map(.x = eval_lambda, .f = ~ list(.x, death_rate, 0.0, catastrophe_params, 0.0, disaster_params))
eval_mu <- seq(from = 0.1, to = 1.3, length = num_eval_steps)
eval_params_mu <- map(.x = eval_mu, .f = ~ list(birth_rate, .x, 0.0, catastrophe_params, 0.0, disaster_params))
eval_params <- c(eval_params_lambda, eval_params_mu)


result <- list(
    simulatedEventsOutputCsv = "out/all-simulated-events.csv",
    simulationParameters = sim_params, # [3.5, 0.1, 0.0, [[1.5,0.2],[2.4,0.3],[3.4,0.3]], 0.0, [[1.0,0.1]]],
    simulationDuration = simulation_duration + 1e-5,
    reconstructedTreeOutputFiles = c("out/reconstructed-newick-tree.txt","out/reconstructed-newick-metadata.csv"),
    observationsOutputCsv = "out/simulated-observations.csv",
    evaluationParameters = eval_params,
    llhdOutputCsv = "out/llhd-evaluations.csv",
    prevalenceDistributionTxt = "out/prevalence-distribution.txt"
)

write_json(result, output_file, pretty = FALSE, auto_unbox = TRUE, digits = 7)

eval_df <- data.frame(parameter = rep(c("lambda", "mu"), each = num_eval_steps),
                      value = c(eval_lambda, eval_mu))

write.table(x = eval_df, file = "out/evaluation-parameters.csv", row.names = FALSE)

true_parameters <- data.frame(parameter = c("lambda", "mu"), value = c(birth_rate,death_rate))

write.table(x = true_parameters, file = "out/true-parameters.csv", row.names = FALSE)
