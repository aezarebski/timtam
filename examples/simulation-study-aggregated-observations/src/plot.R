library(dplyr)
library(reshape2)
library(purrr)
library(magrittr)
library(ggplot2)
library(cowplot)
library(stringr)
library(jsonlite)
library(coda)


green_hex_colour <- "#7fc97f"
purple_hex_colour <- "#beaed4"


SAVE_FIGURES <- TRUE

app_config <- read_json("agg-app-config.json")
sim_duration <- app_config$simulationDuration

sim_param_df <- data.frame(
  lambda = app_config$simulationParameters[[1]],
  psi = app_config$simulationParameters[[3]],
  omega = app_config$simulationParameters[[5]]
)

## =============================================================================
## Generate a figure looking at the posterior samples conditioned upon the
## regular data, i.e., the unscheduled observations.
## =============================================================================

reg_data_mcmc_csv <- app_config$inferenceConfigurations %>%
  extract2(2) %>%
  extract("icMaybeMCMCConfig") %>%
  extract2(1) %>%
  extract2("mcmcOutputCSV")

reg_data_mcmc_df <- read.csv(reg_data_mcmc_csv, stringsAsFactors = FALSE) %>%
  mutate(
    nb_min = qnbinom(p = 0.025, size = nbSize, prob = 1 - nbProb),
    nb_med = qnbinom(p = 0.5, size = nbSize, prob = 1 - nbProb),
    nb_max = qnbinom(p = 0.975, size = nbSize, prob = 1 - nbProb)
  )

reg_data_nb_summary <- reg_data_mcmc_df %>%
  select(starts_with("nb_")) %>%
  colMeans() %>%
  as.list() %>%
  as.data.frame()
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

  sink(file = "out/regular-data-mcmc-diagnostics.txt")
  cat("MCMC diagnostics based on analysis of regular data\n")
  cat("==================================================\n")
  cat("\nSummary\n")
  cat("-------\n")
  print(summary(reg_data_mcmc))
  cat("\n\nThe rejection rate of samples\n")
  cat("-----------------------------\n")
  print(rejectionRate(reg_data_mcmc))
  cat("\n\nThe effective sample size\n")
  cat("-------------------------\n\n")
  print(effectiveSize(reg_data_mcmc))
  sink()
}


reg_data_posterior_df <- read.csv(reg_data_mcmc_csv, stringsAsFactors = FALSE) %>%
  select(lambda, psi, omega)
## Remove the burn in from the start of the samples
reg_data_posterior_df <- tail(reg_data_posterior_df, -1e3)

param_labels <- c(lambda = "Birth rate", psi = "Sequenced sampling rate", omega = "Unsequenced sampling rate")

g1_df <- reg_data_posterior_df %>%
  melt(id.vars = c(), variable.name = "parameter")

g1 <- ggplot() +
  geom_density(data = g1_df,
               mapping = aes(x = value, y = ..density..),
               colour = green_hex_colour,
               size = 1.2) +
  facet_wrap(~parameter,
             scales = "free",
             labeller = labeller(parameter = param_labels)) +
  labs(y = "Posterior density", x = NULL) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 17),
        axis.text = element_text(size = 15),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 17))

fig_height <- 10

if (SAVE_FIGURES) {
  ggsave("out/regular-data-posterior-marginals.png",
    g1,
    height = fig_height,
    width = 2.8 * fig_height,
    units = "cm"
  )
  ggsave("out/regular-data-posterior-marginals.pdf",
    g1,
    height = fig_height,
    width = 2.8 * fig_height,
    units = "cm"
  )
}

## Subsample because otherwise there is massive overplotting which makes it hard
## to grok
g2_df <- reg_data_posterior_df %>% sample_n(size = 2e3)

g2_alpha <- 0.1
g2_size <- 3

g2_a <- ggplot(mapping = aes(x = lambda, y = psi)) +
  geom_vline(xintercept = sim_param_df$lambda, linetype = "dashed") +
  geom_hline(yintercept = sim_param_df$psi, linetype = "dashed") +
  geom_point(data = g2_df, size = g2_size, alpha = g2_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g2_df))), colour = green_hex_colour, size = g2_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["lambda"], y = param_labels["psi"])
g2_b <- ggplot(mapping = aes(x = omega, y = psi)) +
  geom_vline(xintercept = sim_param_df$omega, linetype = "dashed") +
  geom_hline(yintercept = sim_param_df$psi, linetype = "dashed") +
  geom_point(data = g2_df, size = g2_size, alpha = g2_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g2_df))), colour = green_hex_colour, size = g2_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["omega"], y = param_labels["psi"])
g2_c <- ggplot(mapping = aes(x = lambda, y = omega)) +
  geom_vline(xintercept = sim_param_df$lambda, linetype = "dashed") +
  geom_hline(yintercept = sim_param_df$omega, linetype = "dashed") +
  geom_point(data = g2_df, size = g2_size, alpha = g2_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g2_df))), colour = green_hex_colour, size = g2_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["lambda"], y = param_labels["omega"])

g2 <- plot_grid(g2_a, g2_b, g2_c, ncol = 2)

fig_height <- 20

if (SAVE_FIGURES) {
  ggsave("out/regular-data-posterior-joints.png",
    g2,
    height = fig_height,
    width = 1.0 * fig_height,
    units = "cm"
  )
  ggsave("out/regular-data-posterior-joints.pdf",
    g2,
    height = fig_height,
    width = 1.0 * fig_height,
    units = "cm"
  )
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

agg_data_mcmc_df <- read.csv(agg_data_mcmc_csv, stringsAsFactors = FALSE) %>%
  mutate(
    nb_min = qnbinom(p = 0.025, size = nbSize, prob = 1 - nbProb),
    nb_med = qnbinom(p = 0.5, size = nbSize, prob = 1 - nbProb),
    nb_max = qnbinom(p = 0.975, size = nbSize, prob = 1 - nbProb)
  )


agg_data_nb_summary <- agg_data_mcmc_df %>%
  select(starts_with("nb_")) %>%
  colMeans() %>%
  as.list() %>%
  as.data.frame()
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

  sink(file = "out/aggregated-data-mcmc-diagnostics.txt")
  cat("MCMC diagnostics based on analysis of aggregated data\n")
  cat("=====================================================\n")
  cat("\nSummary\n")
  cat("-------\n")
  print(summary(agg_data_mcmc))
  cat("\n\nThe rejection rate of samples\n")
  cat("-----------------------------\n")
  print(rejectionRate(agg_data_mcmc))
  cat("\n\nThe effective sample size\n")
  cat("-------------------------\n\n")
  print(effectiveSize(agg_data_mcmc))
  sink()
}

agg_data_posterior_df <- read.csv(agg_data_mcmc_csv, stringsAsFactors = FALSE) %>%
  select(lambda, rho, nu)
## Remove the burn in from the start of the samples
agg_data_posterior_df <- tail(agg_data_posterior_df, -1e3)

param_labels <- c(lambda = "Birth rate", rho = "Sequenced sampling\nprobability", nu = "Unsequenced sampling\nprobability")

g3_df <- agg_data_posterior_df %>%
  melt(id.vars = c(), variable.name = "parameter")

g3 <- ggplot() +
  geom_density(data = g3_df,
               mapping = aes(x = value, y = ..density..),
               colour = purple_hex_colour,
               size = 1.2) +
  facet_wrap(~parameter,
             scales = "free",
             labeller = labeller(parameter = param_labels)) +
  labs(y = "Posterior density", x = NULL) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 17),
        axis.text = element_text(size = 15),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 17))

fig_height <- 10

if (SAVE_FIGURES) {
  ggsave("out/aggregated-data-posterior-marginals.png",
    g3,
    height = fig_height,
    width = 2.8 * fig_height,
    units = "cm"
  )
  ggsave("out/aggregated-data-posterior-marginals.pdf",
    g3,
    height = fig_height,
    width = 2.8 * fig_height,
    units = "cm"
  )
}


## Subsample because otherwise there is massive overplotting which makes it hard
## to grok
g4_df <- agg_data_posterior_df %>% sample_n(size = 2e3)

g4_alpha <- 0.1
g4_size <- 3

g4_a <- ggplot(mapping = aes(x = lambda, y = rho)) +
  geom_vline(xintercept = sim_param_df$lambda, linetype = "dashed") +
  geom_point(data = g4_df, size = g4_size, alpha = g4_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g4_df))), colour = purple_hex_colour, size = g4_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["lambda"], y = param_labels["rho"])
g4_b <- ggplot(mapping = aes(x = nu, y = rho)) +
  geom_point(data = g4_df, size = g4_size, alpha = g4_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g4_df))), colour = purple_hex_colour, size = g4_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["nu"], y = param_labels["rho"])
g4_c <- ggplot(mapping = aes(x = lambda, y = nu)) +
  geom_vline(xintercept = sim_param_df$lambda, linetype = "dashed") +
  geom_point(data = g4_df, size = g4_size, alpha = g4_alpha) +
  geom_point(data = as.data.frame(as.list(colMeans(g4_df))), colour = purple_hex_colour, size = g4_size + 4, shape = 18) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  labs(x = param_labels["lambda"], y = param_labels["nu"])

g4 <- plot_grid(g4_a, g4_b, g4_c, ncol = 2)


fig_height <- 20

if (SAVE_FIGURES) {
  ggsave("out/aggregated-data-posterior-joints.png",
    g4,
    height = fig_height,
    width = 1.0 * fig_height,
    units = "cm"
  )
  ggsave("out/aggregated-data-posterior-joints.pdf",
    g4,
    height = fig_height,
    width = 1.0 * fig_height,
    units = "cm"
  )
}
## =============================================================================
## Generate a figure looking at the prevalence through time and the data used in
## the inference.
## =============================================================================
all_events <- read.csv("out/all-simulated-events.csv", header = FALSE, stringsAsFactors = FALSE) %>%
  select(V1, V2) %>%
  set_names(c("event", "abs_time"))

update_prev <- function(n, e) {
  switch(EXPR = as.character(e),
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

regular_data <- read.csv("out/simulated-observations-true-params-regular-data.csv", header = FALSE, stringsAsFactors = FALSE) %>%
  set_names(c("delay", "observed_event")) %>%
  mutate(abs_time = cumsum(delay))

update_reg_data_ltt <- function(n, e) {
  switch(EXPR = as.character(e),
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

aggregated_data <- read.csv("out/simulated-observations-est-params-agg-data.csv", header = FALSE, stringsAsFactors = FALSE) %>%
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
