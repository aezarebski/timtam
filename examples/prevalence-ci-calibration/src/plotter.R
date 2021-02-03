library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)
library(reshape2)
library(jsonlite)


vis_data <- read_json("demo.json")

r_naught_and_prevalence_record <- function(sim_result) {
  sim_seed <- sim_result$simulationSeed
  r_naught <- sim_result$regularParameterEstimates %>%
    keep(~ .x$name == "rNaught") %>%
    flatten()
  data.frame(
    seed = sim_seed,
    r_naught_est = r_naught$estimate,
    r_naught_lower = r_naught$credibleInterval[[1]],
    r_naught_upper = r_naught$credibleInterval[[2]]
  )
}

## This is the data frame that produces the figure in the main text.
r_naught_and_prevalence_df <- map(vis_data$simulationResults,
                                  r_naught_and_prevalence_record) %>%
  bind_rows



## =============================================================================

make_figures <- function(vis_data) {

}

main <- function(args) {
  vis_data_json <- as.character(args[1])

  if (file.exists(vis_data_json)) {
    vis_data <- read_json(vis_data_json)
    make_figures(vis_data)
  } else {
    stop("Could not find visualisation data JSON.")
  }
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  main(args)
}
