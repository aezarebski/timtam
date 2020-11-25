library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(jsonlite)


## =============================================================================
## Generate cross sections of the LLHD function in the birth rate.
## =============================================================================
x <- list(
  "out/llhd-evaluations-est-params-agg-data.csv",
  "out/llhd-evaluations-est-params-regular-data.csv",
  "out/llhd-evaluations-true-params-regular-data.csv"
)

app_config <- read_json("agg-app-config.json")
sim_lambda <- app_config$simulationParameters[[1]]

read_llhds <- function(filename) {
  if (file.exists(filename)) {
    filename %>%
      read.csv(header = FALSE) %>%
      set_names(c("type", "value")) %>%
      mutate(lambda = seq(from = 1, to = 3.5, length = 100))
  } else {
    NULL
  }
}

y <- map(
  x,
  read_llhds
) %>%
  keep(compose(not, is_null)) %>%
  bind_rows()

g <- ggplot(y) +
  geom_line(mapping = aes(x = lambda, y = value)) +
  geom_vline(xintercept = sim_lambda) +
  facet_wrap(~type, scales = "free_y")

ggsave("scratch-output-1.png", g)


## =============================================================================
## Generate a figure looking at the posterior distribution of the prevalence
## =============================================================================
x <- list(
  "out/final-negative-binomial-est-params-agg-data.csv",
  "out/final-negative-binomial-est-params-regular-data.csv",
  "out/final-negative-binomial-true-params-regular-data.csv"
)

read_nb <- function(filename) {
  if (file.exists(filename)) {
    foo <- filename %>%
      readLines() %>%
      str_split(pattern = "(,| )") %>%
      unlist()

    percentile_probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
    percentile_vals <- qnbinom(
      p = c(0.025, 0.25, 0.5, 0.75, 0.975),
      size = as.numeric(foo[3]),
      prob = 1 - as.numeric(foo[4])
    )
    estimate_type <- foo[1]

    data.frame(
      percentile_prob = percentile_probs,
      percentile_value = percentile_vals,
      estimate_name = estimate_type
    )
  } else {
    NULL
  }
}

y <- map(x, read_nb) %>%
  keep(compose(not, is_null)) %>%
  bind_rows()

g <- ggplot(y) +
  geom_point(mapping = aes(
    x = estimate_name,
    y = percentile_value,
    colour = percentile_prob
  )) +
  ylim(c(0, 1.1 * max(y$percentile_value)))


ggsave("scratch-output-2.png", g)

## =============================================================================
## Generate a figure looking at the prevalence through time and the data used in
## the inference.
## =============================================================================
all_events <- read.csv("out/all-simulated-events.csv", header = FALSE) %>%
  select(V1, V2) %>%
  set_names(c("event", "abs_time"))

update_prev <- function(n, e) {
  switch(EXPR = e,
    infection = n + 1,
    occurrence = n - 1,
    removal = n - 1,
    sampling = n - 1
  )
}

prev_df <- data.frame(
  absolute_time = c(0, all_events$abs_time),
  prevalence = accumulate(
    .x = all_events$event,
    .f = update_prev, .init = 1
  )
)

regular_data <- read.csv("out/simulated-observations-true-params-regular-data.csv", header = FALSE) %>%
  set_names(c("delay", "observed_event")) %>%
  mutate(abs_time = cumsum(delay))

update_reg_data_ltt <- function(n, e) {
  switch(EXPR = e,
    obirth = n + 1,
    osample = n - 1
  )
}

just_tree_obs <- regular_data %>%
  filter(observed_event != "ooccurrence") %>%
  select(-delay)

reg_tree_df <- data.frame(
  absolute_time = c(0, just_tree_obs$abs_time),
  ltt = accumulate(
    .x = just_tree_obs$observed_event,
    .f = update_reg_data_ltt,
    .init = 1
  )
)

occ_df <- regular_data %>%
  filter(observed_event == "ooccurrence") %>%
  select(-delay) %>%
  rename(absolute_time = abs_time)

g <- ggplot() +
  geom_step(data = prev_df, mapping = aes(x = absolute_time, y = prevalence)) +
  geom_step(data = reg_tree_df, mapping = aes(x = absolute_time, y = ltt), colour = "green") +
  geom_histogram(data = occ_df, mapping = aes(x = absolute_time), fill = "green", alpha = 0.1, colour = "green")

ggsave("scratch-output-3.png", g)
