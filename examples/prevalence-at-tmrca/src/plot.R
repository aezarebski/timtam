library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(reshape2)
library(jsonlite)
library(coda)

#' Return a ggplot figure showing the estimates of the prevalence.
prevalence_estimate_figure <- function(epi_events_df, mcmc_df, tmrca) {
  update_prev <- function(prev, event) {
    switch(EXPR = as.character(event),
      infection = prev + 1,
      occurrence = prev - 1,
      removal = prev - 1,
      sampling = prev - 1
    )
  }

  prev_df <- data.frame(
    absolute_time = c(0, epi_events_df$abs_time),
    prevalence = accumulate(
      .x = epi_events_df$event,
      .f = update_prev, .init = 1
    )
  )

  tmrca_prev_df <- mcmc_df %>%
    mutate(
      nbProb = nbMean / nbVar,
      nbSize = nbMean^2 / (nbVar - nbMean),
      nbUpper95 = qnbinom(p = 0.975, size = nbSize, prob = nbProb),
      nbMid = qnbinom(p = 0.5, size = nbSize, prob = nbProb),
      nbLower95 = qnbinom(p = 0.025, size = nbSize, prob = nbProb),
      tmrca = tmrca
    ) %>%
    colMeans() %>%
    as.list() %>%
    as.data.frame()

  ggplot() +
    geom_step(
      data = prev_df,
      mapping = aes(x = absolute_time, y = prevalence)
    ) +
    geom_errorbar(
      data = tmrca_prev_df,
      mapping = aes(x = tmrca, ymin = nbLower95, ymax = nbUpper95)
    ) +
    geom_point(
      data = tmrca_prev_df,
      mapping = aes(x = tmrca, y = nbMid)
    ) +
    scale_y_log10() +
    theme_classic()
}


#' Run some basic diagnostics over the MCMC samples and save the results.
run_mcmc_diagnostics <- function(mcmc_df) {
  mcmc_obj <- as.mcmc(mcmc_df)

  png("out/mcmc-trace.png")
  plot(mcmc_obj)
  dev.off()

  sink("out/mcmc-diagnostics.txt")
  cat("\n===========================================================\n")
  cat("Summary\n")
  cat("===========================================================\n")
  print(summary(mcmc_obj))
  cat("\n===========================================================\n")
  cat("Effective Size\n")
  cat("===========================================================\n")
  print(effectiveSize(mcmc_obj))
  sink()

  return(0)
}

main <- function(args) {
  input_file <- args[1]
  if (file.exists(input_file)) {
    cat(sprintf("Reading configuration from file: %s\n", input_file))
    input_file <- "app-config.json"
    app_config <- read_json(input_file)
    epi_events_csv <- app_config$acEpiEventsCsv
    additional_vals <- read_json(app_config$acAdditionalJson)
    tmrca <- additional_vals$avTmrcaFromZero
    mcmc_csv <- app_config$acMCMCConfig$mcmcOutputCSV
    mcmc_df <- read.csv(mcmc_csv)

    epi_events_df <- read.csv(epi_events_csv, header = FALSE) %>%
      select(V1, V2) %>%
      set_names(c("event", "abs_time"))

    run_mcmc_diagnostics(mcmc_df)

    g <- prevalence_estimate_figure(epi_events_df, mcmc_df, tmrca)
    ggsave("out/prevalence-estimates.png", g)
  } else {
    stop(sprintf("Cannot find configuration file: %s", input_file))
  }
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}
