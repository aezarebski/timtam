library(ggplot2)
library(jsonlite)
library(purrr)

config <- read_json("ts-config.json")

eval_df <- read.table("out/evaluation-parameters.csv", header = TRUE)
eval_df$llhd <- as.double(as_vector(strsplit(x = readLines(config$llhdOutputCsv), split = ",")))

true_parameters <- read.table("out/true-parameters.csv", header = TRUE)

profiles_fig <- ggplot(eval_df, mapping = aes(x = value, y = llhd)) +
    geom_line() +
    geom_vline(data = true_parameters, mapping = aes(xintercept = value), linetype = "dashed") +
    facet_wrap(~parameter, scales = "free") +
    theme_classic()

ggsave("out/llhd-profiles.png", profiles_fig)
