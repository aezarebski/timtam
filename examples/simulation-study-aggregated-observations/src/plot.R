library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(jsonlite)
library(coda)

green_hex_colour <- "#7fc97f"
purple_hex_colour <- "#beaed4"


SAVE_FIGURES <- TRUE

app_config <- read_json("agg-app-config.json")
sim_lambda <- app_config$simulationParameters[[1]]
sim_duration <- app_config$simulationDuration


## =============================================================================
## Generate a figure looking at the posterior samples conditioned upon the
## regular data, i.e., the unscheduled observations.
## =============================================================================

reg_data_mcmc_csv <- app_config$inferenceConfigurations %>%
  extract2(2) %>%
  extract("icMaybeMCMCConfig") %>%
  extract2(1) %>%
  extract2("mcmcOutputCSV")

reg_data_mcmc_df <- read.csv(reg_data_mcmc_csv) %>%
  mutate(nb_min = qnbinom(p = 0.025, size = nbSize, prob = 1 - nbProb),
         nb_med = qnbinom(p = 0.5, size = nbSize, prob = 1 - nbProb),
         nb_max = qnbinom(p = 0.975, size = nbSize, prob = 1 - nbProb))

reg_data_nb_summary <- reg_data_mcmc_df %>% select(starts_with("nb_")) %>% colMeans %>% as.list %>% as.data.frame
reg_data_nb_summary$absolute_time <- sim_duration

small_mcmc_subset <- if (nrow(reg_data_mcmc_df) > 1000) {
  sample_n(reg_data_mcmc_df, 1000)
} else {
  reg_data_mcmc_df
}

if (SAVE_FIGURES) {
  png("out/regular-data-mcmc-pairs-plot.png")
  pairs(select(small_mcmc_subset, llhd, lambda, psi, omega))
  dev.off()
}

reg_data_mcmc <- mcmc(select(reg_data_mcmc_df, llhd, lambda, psi, omega))

if (SAVE_FIGURES) {
  png("out/regular-data-mcmc-trace.png")
  plot(reg_data_mcmc)
  dev.off()
}

## =============================================================================
## Generate a figure looking at the posterior samples conditioned upon the
## aggregated data, i.e., the observations that have been aggregated into
## scheduled observations.
## =============================================================================

agg_data_mcmc_csv <- app_config$inferenceConfigurations %>%
  extract2(3) %>%
  extract("icMaybeMCMCConfig") %>%
  extract2(1) %>%
  extract2("mcmcOutputCSV")

agg_data_mcmc_df <- read.csv(agg_data_mcmc_csv) %>%
  mutate(nb_min = qnbinom(p = 0.025, size = nbSize, prob = 1 - nbProb),
         nb_med = qnbinom(p = 0.5, size = nbSize, prob = 1 - nbProb),
         nb_max = qnbinom(p = 0.975, size = nbSize, prob = 1 - nbProb))


agg_data_nb_summary <- agg_data_mcmc_df %>% select(starts_with("nb_")) %>% colMeans %>% as.list %>% as.data.frame
agg_data_nb_summary$absolute_time <- sim_duration

small_mcmc_subset <- if (nrow(agg_data_mcmc_df) > 1000) {
                       sample_n(agg_data_mcmc_df, 1000)
                     } else {
                       agg_data_mcmc_df
                     }


if (SAVE_FIGURES) {
  png("out/aggregated-data-mcmc-pairs-plot.png")
  pairs(select(small_mcmc_subset, llhd, lambda, rho, nu))
  dev.off()
}

agg_data_mcmc <- mcmc(select(agg_data_mcmc_df, llhd, lambda, rho, nu))

if (SAVE_FIGURES) {
  png("out/aggregated-data-mcmc-trace.png")
  plot(agg_data_mcmc)
  dev.off()
}

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


g <- ggplot() +
  geom_step(data = prev_df, mapping = aes(x = absolute_time, y = prevalence)) +
  geom_step(data = reg_tree_df, mapping = aes(x = absolute_time, y = ltt), colour = green_hex_colour) +
  geom_histogram(data = occ_df, mapping = aes(x = absolute_time), fill = green_hex_colour, alpha = 0.1, colour = green_hex_colour) +
  geom_step(data = agg_tree_df, mapping = aes(x = absolute_time, y = ltt), colour = purple_hex_colour) +
  geom_segment(data = agg_occ_df, mapping = aes(x = absolute_time, y = num_obs, xend = absolute_time, yend = 0), colour = purple_hex_colour) +
  geom_point(data = agg_occ_df, mapping = aes(x = absolute_time, y = num_obs), colour = purple_hex_colour) +
  geom_errorbar(
    data = reg_data_nb_summary,
    mapping = aes(x = absolute_time - 0.1, ymin = nb_min, ymax = nb_max),
    colour = green_hex_colour, linetype = "solid", width = 0.2
  ) +
  geom_point(
    data = reg_data_nb_summary,
    mapping = aes(x = absolute_time - 0.1, y = nb_med),
    colour = green_hex_colour
  ) +
  geom_errorbar(
    data = agg_data_nb_summary,
    mapping = aes(x = absolute_time + 0.1, ymin = nb_min, ymax = nb_max),
    colour = purple_hex_colour, linetype = "solid", width = 0.2
  ) +
  geom_point(
    data = agg_data_nb_summary,
    mapping = aes(x = absolute_time + 0.1, y = nb_med),
    colour = purple_hex_colour
  ) +
  labs(y = NULL, x = "Time since origin") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold"))

fig_height <- 10

if (SAVE_FIGURES) {
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
}


