library(dplyr)
library(reshape2)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)

mesh <- function(a,b) {
  seq(from = a, to = b, length = 100)
}

param_values <- c(mesh(1,2.5),mesh(0.05, 1.5),mesh(0.05, 1.5))
param_names <- rep(c("lambda", "psi", "mu"), each = 100)


read_llhd_profile <- function(file_path) {
  readLines(file_path) %>%
    str_split(",") %>%
    unlist %>%
    tail(-1) %>%
    as.numeric
}

true_llhds <- read_llhd_profile("out-backup/llhd-evaluations-true-params-regular-data.csv")
est_reg_llhds <- read_llhd_profile("out-backup/llhd-evaluations-est-params-regular-data.csv")

true_params_reg_data_df <- data.frame(
  parameter_name = param_names,
  parameter_value = param_values,
  parameter_kind = "true unscheduled",
  llhd = true_llhds
)

est_params_reg_data_df <- data.frame(
  parameter_name = param_names,
  parameter_value = param_values,
  parameter_kind = "estimated unscheduled",
  llhd = est_reg_llhds
)

llhd_profiles_df <- rbind(true_params_reg_data_df,est_params_reg_data_df)


config <- jsonlite::read_json("agg-app-config.json") %>%
  use_series("simulationParameters") %>%
  set_names(c("lambda", "mu", "psi", "timedRhos", "omega", "timedNus"))
config_df <- data.frame(parameter_name = names(config)[1:3],
                        parameter_value = unlist(config)[1:3],
                        parameter_kind = "true unscheduled")

ggplot(llhd_profiles_df,
       aes(x = parameter_value,
           y = llhd)) +
  geom_line() +
  geom_vline(data = config_df,
             mapping = aes(xintercept = parameter_value)) +
  facet_grid(parameter_kind~parameter_name, scales = "free_x")

ggsave("llhd-profiles.png")
