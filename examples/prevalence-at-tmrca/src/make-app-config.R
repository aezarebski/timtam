library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(reshape2)
library(jsonlite)



mcmc_config <- list(
  mcmcOutputCSV = "demoC.csv",
  mcmcNumIters = 100,
  mcmcStepSD = 0.1,
  mcmcSeed = 7
)

app_config <- list(
  acEpiEventsCsv = "demoA.csv",
  acObservationsCsv = "demoB.csv",
  acMCMCConfig = mcmc_config
)


main <- function(args) {
  output_file <- args[1]
  cat("\nWriting configuration to ", output_file, "\n")
  write_json(app_config,
    path = output_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = 6
  )
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}
