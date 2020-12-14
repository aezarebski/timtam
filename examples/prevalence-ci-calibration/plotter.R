library(dplyr)
library(reshape2)
library(jsonlite)
library(ggplot2)
library(purrr)
library(magrittr)
library(coda)

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

  sim_params <- app_config$simulationParameters
  names(sim_params) <- c("lambda", "mu", "psi", "rhoProbs", "omega", "nuProbs")

  tmp <- select(mcmc_df, lambda, psi, omega)
  tmp$r_naught <- tmp$lambda / (sim_params$mu + tmp$psi + tmp$omega)
  summary_func <- function(x) quantile(x, probs = c(0.025, 0.5, 0.975))
  tmp <- data.frame(
    value = c(summary_func(tmp$lambda), summary_func(tmp$psi), summary_func(tmp$omega), summary_func(tmp$r_naught)),
    param = rep(c("lambda", "psi", "omega", "r_naught"), each = 3),
    statistic = rep(c("min", "mid", "max"), 4),
    sim_seed = rep(sim_seed, 12)
  )
  write.table(
    x = tmp,
    file = sprintf("%s/param-summary-%d.csv", output_dir, sim_seed),
    sep = ",",
    row.names = FALSE
  )
  rm(tmp, summary_func)

  nb_summary <- mcmc_df %>%
    select(starts_with("nb_")) %>%
    colMeans() %>%
    as.list() %>%
    as.data.frame()
  nb_summary$absolute_time <- sim_duration


  g <- ggplot() +
    geom_step(
      data = prev_df,
      mapping = aes(x = absolute_time, y = prevalence)
    ) +
    geom_errorbar(
      data = nb_summary,
      mapping = aes(x = absolute_time, ymin = nb_min, y = nb_med, ymax = nb_max),
      colour = green_hex_colour
    ) +
    geom_point(
      data = nb_summary,
      mapping = aes(x = absolute_time, ymin = nb_min, y = nb_med, ymax = nb_max),
      colour = green_hex_colour
    )

  ggsave(sprintf("%s/summary-figure-%d.png", output_dir, sim_seed), g)


  result <- nb_summary
  result$true_final_prevalence <- prev_df$prevalence %>% tail(1)


  write.table(
    x = result,
    file = sprintf("%s/summary-seed-%d.csv", output_dir, sim_seed),
    sep = ",",
    row.names = FALSE
  )

  mcmc_obj <- read.csv(mcmc_csv) %>%
    select(lambda, psi, omega) %>%
    as.mcmc()
  png(sprintf("%s/mcmc-trace-%d.png", output_dir, sim_seed))
  plot(mcmc_obj)
  dev.off()
}


main <- function(args) {
  num_seeds <- as.integer(args[1])

  if (and(is.integer(num_seeds), num_seeds > 0)) {
    for (sim_seed in 1:num_seeds) {
      run_post_processing(sim_seed)
    }

    .read_csv_from_seed <- function(sim_seed) {
      read.csv(sprintf(
        "out/seed-%d/summary-seed-%d.csv",
        sim_seed, sim_seed
      ))
    }
    plot_df <- lapply(1:num_seeds, .read_csv_from_seed) %>% bind_rows()
    plot_df <- plot_df[order(plot_df$true_final_prevalence), ]
    plot_df$order <- 1:num_seeds


    g <- ggplot() +
      geom_point(
        data = plot_df,
        mapping = aes(x = order, y = true_final_prevalence)
      ) +
      geom_errorbar(
        data = plot_df,
        mapping = aes(x = order, ymin = nb_min, y = nb_med, ymax = nb_max),
        colour = green_hex_colour
      )

    ggsave("replication-results-prevalence.png", g)



    config <- read_json("out/seed-1/config-1.json")
    sim_params <- config$simulationParameters
    names(sim_params) <- c("lambda", "mu", "psi", "rhoProbs", "omega", "nuProbs")

    .read_csv_param_summary <- function(sim_seed) {
      read.csv(sprintf("out/seed-%d/param-summary-%d.csv", sim_seed, sim_seed))
    }
    params_df <- lapply(1:num_seeds, .read_csv_param_summary) %>% bind_rows()

    lambda_df <- params_df %>% filter(param == "lambda") %>% select(value, statistic, sim_seed) %>% dcast(sim_seed ~ statistic)
    g_lambda <- ggplot(lambda_df) +
      geom_errorbar(mapping = aes(x = sim_seed, ymin = min, ymax = max), colour = green_hex_colour) +
      geom_hline(yintercept = sim_params$lambda, linetype = "dashed")
    ggsave("replication-results-lambda.png", g_lambda)


    psi_df <- params_df %>% filter(param == "psi") %>% select(value, statistic, sim_seed) %>% dcast(sim_seed ~ statistic)
    g_psi <- ggplot(psi_df) +
      geom_errorbar(mapping = aes(x = sim_seed, ymin = min, ymax = max), colour = green_hex_colour) +
      geom_hline(yintercept = sim_params$psi, linetype = "dashed")
    ggsave("replication-results-psi.png", g_psi)


    omega_df <- params_df %>% filter(param == "omega") %>% select(value, statistic, sim_seed) %>% dcast(sim_seed ~ statistic)
    g_omega <- ggplot(omega_df) +
      geom_errorbar(mapping = aes(x = sim_seed, ymin = min, ymax = max), colour = green_hex_colour) +
      geom_hline(yintercept = sim_params$omega, linetype = "dashed")
    ggsave("replication-results-omega.png", g_omega)


    r_naught_df <- params_df %>% filter(param == "r_naught") %>% select(value, statistic, sim_seed) %>% dcast(sim_seed ~ statistic)
    g_r_naught <- ggplot(r_naught_df) +
      geom_errorbar(mapping = aes(x = sim_seed, ymin = min, ymax = max), colour = green_hex_colour) +
      geom_hline(yintercept = sim_params$lambda / (sim_params$mu + sim_params$psi + sim_params$omega), linetype = "dashed")
    ggsave("replication-results-r-naught.png", g_r_naught)

  } else {
    stop("Could not get num_seeds from command line argument.")
  }
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}
