library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(reshape2)
library(jsonlite)
library(coda)


#' Run some basic diagnostics over the MCMC samples and save the results.
run_mcmc_diagnostics <- function(mcmc_csv) {
  mcmc_df <- read.csv(mcmc_csv)
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
    app_config <- read_json(input_file)

    mcmc_csv <- app_config$acMCMCConfig$mcmcOutputCSV
    run_mcmc_diagnostics(mcmc_csv)

    additional_vals <- read_json(app_config$acAdditionalJson)
  } else {
    stop(sprintf("Cannot find configuration file: %s", input_file))
  }
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}
