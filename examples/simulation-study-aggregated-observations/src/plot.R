library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(jsonlite)

green_hex_colour <- "#7fc97f"
purple_hex_colour <- "#beaed4"


app_config <- read_json("agg-app-config.json")
sim_lambda <- app_config$simulationParameters[[1]]

## =============================================================================
## Generate a figure looking at the posterior distribution of the prevalence
## =============================================================================
data_paths <- list(
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

posterior_plot_df <- map(data_paths, read_nb) %>%
  keep(compose(not, is_null)) %>%
  bind_rows()

g <- ggplot(posterior_plot_df) +
  geom_point(mapping = aes(
    x = estimate_name,
    y = percentile_value,
    colour = percentile_prob
  )) +
  ylim(c(0, 1.1 * max(posterior_plot_df$percentile_value)))


ggsave("out/posterior-prevelance-estimates.png", g)

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

## -----------------------------------------------------------------------------

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

## -----------------------------------------------------------------------------

aggregated_data <- read.csv("out/simulated-observations-est-params-agg-data.csv", header = FALSE) %>%
  set_names(c("delay", "observed_event")) %>%
  mutate(abs_time = cumsum(delay))

just_agg_tree_obs <- aggregated_data %>%
  filter(str_detect(string = observed_event, pattern = "odisaster", negate = TRUE)) %>%
  select(-delay)

update_agg_data_ltt <- function(n, e) {
  if (e == "obirth") {
    n + 1
  } else {
    n - (e %>% str_split(pattern = ":") %>% unlist() %>% extract(2) %>% as.numeric())
  }
}


agg_tree_df <- data.frame(
  absolute_time = c(0, just_agg_tree_obs$abs_time),
  ltt = accumulate(
    .x = just_agg_tree_obs$observed_event,
    .f = update_agg_data_ltt,
    .init = 1
  )
)

agg_occ_df <- aggregated_data %>%
  filter(str_detect(string = observed_event, pattern = "odisaster")) %>%
  select(-delay) %>%
  rename(absolute_time = abs_time) %>%
  mutate(num_obs = observed_event %>% str_split(pattern = ":") %>% map(extract(2)) %>% unlist() %>% as.numeric())

## -----------------------------------------------------------------------------

sim_duration <- app_config$simulationDuration

tmp_true_regular <- filter(posterior_plot_df, estimate_name == "true_parameters_regular_data") %>% use_series("percentile_value")
true_regular_df <- data.frame(absolute_time = sim_duration, prev_est_min = min(tmp_true_regular), prev_est_mid = median(tmp_true_regular), prev_est_max = max(tmp_true_regular))

tmp_est_regular <- filter(posterior_plot_df, estimate_name == "estimated_parameters_regular_data") %>% use_series("percentile_value")
est_regular_df <- data.frame(absolute_time = sim_duration, prev_est_min = min(tmp_est_regular), prev_est_mid = median(tmp_est_regular), prev_est_max = max(tmp_est_regular))

tmp_est_aggregated <- filter(posterior_plot_df, estimate_name == "estimated_parameters_aggregated_data") %>% use_series("percentile_value")
est_aggregated_df <- data.frame(absolute_time = sim_duration, prev_est_min = min(tmp_est_aggregated), prev_est_mid = median(tmp_est_aggregated), prev_est_max = max(tmp_est_aggregated))





g <- ggplot() +
  geom_step(data = prev_df, mapping = aes(x = absolute_time, y = prevalence)) +
  geom_step(data = reg_tree_df, mapping = aes(x = absolute_time, y = ltt), colour = green_hex_colour) +
  geom_histogram(data = occ_df, mapping = aes(x = absolute_time), fill = green_hex_colour, alpha = 0.1, colour = green_hex_colour) +
  geom_step(data = agg_tree_df, mapping = aes(x = absolute_time, y = ltt), colour = purple_hex_colour) +
  geom_segment(data = agg_occ_df, mapping = aes(x = absolute_time, y = num_obs, xend = absolute_time, yend = 0), colour = purple_hex_colour) +
  geom_point(data = agg_occ_df, mapping = aes(x = absolute_time, y = num_obs), colour = purple_hex_colour) +
  geom_errorbar(
    data = true_regular_df,
    mapping = aes(x = absolute_time - 0.1, ymin = prev_est_min, ymax = prev_est_max),
    colour = green_hex_colour, linetype = "dashed", width = 0.2
  ) +
  geom_point(
    data = true_regular_df,
    mapping = aes(x = absolute_time - 0.1, y = prev_est_mid),
    colour = green_hex_colour
  ) +
  geom_errorbar(
    data = est_regular_df,
    mapping = aes(x = absolute_time, ymin = prev_est_min, ymax = prev_est_max),
    colour = green_hex_colour, linetype = "solid", width = 0.2
  ) +
  geom_point(
    data = est_regular_df,
    mapping = aes(x = absolute_time, y = prev_est_mid),
    colour = green_hex_colour
  ) +
  geom_errorbar(
    data = est_aggregated_df,
    mapping = aes(x = absolute_time + 0.1, ymin = prev_est_min, ymax = prev_est_max),
    colour = purple_hex_colour, linetype = "solid", width = 0.2
  ) +
  geom_point(
    data = est_aggregated_df,
    mapping = aes(x = absolute_time + 0.1, y = prev_est_mid),
    colour = purple_hex_colour
  ) +
  labs(y = NULL, x = "Time since origin") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"))

fig_height <- 10
ggsave("out/regular-and-aggregated-data.png",
  g,
  height = fig_height,
  width = 1.618 * fig_height,
  units = "cm"
)
ggsave("out/regular-and-aggregated-data.pdf",
  g,
  height = fig_height,
  width = 1.618 * fig_height,
  units = "cm"
)
