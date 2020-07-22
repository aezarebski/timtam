library(ggplot2)
library(dplyr)
library(jsonlite)
library(purrr)

config <- read_json("ts-config.json")



## Make profile plots of the LLHD function.
eval_df <- read.table("out/evaluation-parameters.csv",
                      header = TRUE)
eval_df$llhd <- as.double(as_vector(strsplit(x = readLines(config$llhdOutputCsv), split = ",")))

true_parameters <- read.table("out/true-parameters.csv",
                              header = TRUE)

profiles_fig <- ggplot(eval_df, mapping = aes(x = value, y = llhd)) +
    geom_line() +
    geom_vline(data = true_parameters, mapping = aes(xintercept = value), linetype = "dashed") +
    facet_wrap(~parameter, scales = "free") +
    theme_classic()

ggsave("out/llhd-profiles.png", profiles_fig)






## Plot the posterior distribution of the prevalence given the true parameters.
primary_count <- function(ps) {
    unlist(map(strsplit(x = ps, split = ":"), length))
}

all_events <- read.csv("out/all-simulated-events.csv",
                       header = FALSE,
                       stringsAsFactors = FALSE) %>%
    set_names(c("event", "time", "primary", "secondary")) %>%
    mutate(delta = primary_count(primary)) %>%
    select(event,time,delta)
all_events$population_size <- cumsum(ifelse(all_events$event == "infection", +1, -1) * all_events$delta)
final_prevalence <- tail(all_events$population_size, 1)

nb_params <- readLines("out/prevalence-distribution.txt") %>%
    strsplit(split = " ") %>%
    unlist %>%
    tail(2) %>%
    as.double %>%
    as.list %>%
    set_names(c("size", "prob"))

prev_vals <- 0:100
prev_post_probs <- dnbinom(x = prev_vals, size = nb_params$size, prob = nb_params$prob)

plot_df <- data.frame(prevalence = prev_vals,
                      posterior = prev_post_probs)

prev_fig <- ggplot(plot_df, aes(x = prevalence, y = posterior)) +
    geom_line() +
    geom_vline(xintercept = final_prevalence, linetype = "dashed") +
    theme_classic()

ggsave("out/prevalence-profiles.png", prev_fig)
