library(dplyr)
library(jsonlite)
library(ggplot2)
library(purrr)
library(magrittr)

green_hex_colour <- "#7fc97f"







run_post_processing <- function(sim_seed) {
  output_dir <- sprintf("out/seed-%d", sim_seed)

  all_events_csv <- sprintf("%s/all-simulated-events.csv", output_dir)

  all_events <- read.csv(all_events_csv,
    header = FALSE,
    stringsAsFactors = FALSE
  ) %>%
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

  app_config <- read_json(sprintf("%s/config-%d.json", output_dir, sim_seed))
  sim_duration <- app_config$simulationDuration

  mcmc_csv <- app_config$inferenceConfigurations %>%
    extract2(2) %>%
    extract("icMaybeMCMCConfig") %>%
    extract2(1) %>%
    extract2("mcmcOutputCSV")

  mcmc_df <- read.csv(mcmc_csv, stringsAsFactors = FALSE) %>%
    mutate(
      nb_min = qnbinom(p = 0.025, size = nbSize, prob = 1 - nbProb),
      nb_med = qnbinom(p = 0.5, size = nbSize, prob = 1 - nbProb),
      nb_max = qnbinom(p = 0.975, size = nbSize, prob = 1 - nbProb)
    )

  nb_summary <- mcmc_df %>%
    select(starts_with("nb_")) %>%
    colMeans() %>%
    as.list() %>%
    as.data.frame()
  nb_summary$absolute_time <- sim_duration


  g <- ggplot() +
    geom_step(data = prev_df, mapping = aes(x = absolute_time, y = prevalence)) +
    geom_errorbar(data = nb_summary, mapping = aes(x = absolute_time, ymin = nb_min, y = nb_med, ymax = nb_max), colour = green_hex_colour) +
    geom_point(data = nb_summary, mapping = aes(x = absolute_time, ymin = nb_min, y = nb_med, ymax = nb_max), colour = green_hex_colour)

  ggsave(sprintf("%s/summary-figure-%d.png", output_dir, sim_seed), g)


  result <- nb_summary
  result$true_final_prevalence <- prev_df$prevalence %>% tail(1)


  write.table(
    x = result,
    file = sprintf("%s/summary-seed-%d.csv", output_dir, sim_seed),
    sep = ",",
    row.names = FALSE
  )
}

for (sim_seed in 1:10) {
  run_post_processing(sim_seed)
}



plot_df <- lapply(1:10, function(sim_seed) read.csv(sprintf("out/seed-%d/summary-seed-%d.csv", sim_seed, sim_seed))) %>% bind_rows
plot_df <- plot_df[order(plot_df$true_final_prevalence),]
plot_df$order <- 1:10


ggplot() +
  geom_point(data = plot_df, mapping = aes(x = order, y = true_final_prevalence)) +
  geom_errorbar(data = plot_df, mapping = aes(x = order, ymin = nb_min, y = nb_med, ymax = nb_max), colour = green_hex_colour)
