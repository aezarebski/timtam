library(argparse)
library(coda)
library(reshape2)
library(dplyr)

parser <- ArgumentParser()

parser$add_argument(
         "-n",
         "--num-replicates",
         type = "integer",
         default = 1,
         help = "Number of replicates to carry out."
       )
parser$add_argument(
         "-p",
         "--parameters",
         type = "character",
         help = "Filepath to parameters JSON"
       )
parser$add_argument(
         "--burnin",
         type = "integer",
         help = "Number of samples to discard as burnin."
       )
parser$add_argument(
         "--output",
         type = "character",
         help = "Filepath to write JSON results."
       )

## put the summary data in a tidy format for export
write_summary <- function(summaries, summaries_from_agg, parameters, output) {
  prevalences <- do.call(rbind, lapply(summaries, function(s) data.frame(replicate = s$replicate, prevalence = s$final_prevalence)))

  estimates_not_agg <- do.call(rbind, lapply(summaries, function(s) {
    tmp <- s$estimates
    tmp$replicate <- s$replicate
    tmp$from_aggregated <- FALSE
    return(tmp)
  }))

  estimates_agg <- do.call(rbind, lapply(summaries_from_agg, function(s) {
    tmp <- s$estimates
    tmp$replicate <- s$replicate
    tmp$from_aggregated <- TRUE
    return(tmp)
  }))

  estimates <- rbind(estimates_not_agg, estimates_agg)

  diags_not_agg <- do.call(rbind, lapply(summaries, function(s) {
    tmp <- as.data.frame(s$diagnostics$effective_size)
    tmp$replicate <- s$replicate
    tmp$from_aggregated <- FALSE
    return(tmp)
  }))

  diags_agg <- do.call(rbind, lapply(summaries_from_agg, function(s) {
    tmp <- as.data.frame(s$diagnostics$effective_size)
    tmp$replicate <- s$replicate
    tmp$from_aggregated <- TRUE
    return(tmp)
  }))

  result <- list(prevalence = prevalences,
                 estimates = estimates,
                 diagnostics = list(effectiveSize = list(aggregated = diags_agg, not_aggregated = diags_not_agg)))

  jsonlite::write_json(
              x = result,
              path = output,
              auto_unbox = T,
              pretty = TRUE
            )
}

## read the MCMC samples and return some summary statistics
read_and_summarise <- function(replicate_ix, num_burnin, from_aggregated, death_rate) {
  final_prev <- jsonlite::read_json(sprintf(
                            "out/replicate-%d/ape-sim-final-prevalence.json",
                            replicate_ix
                          ), simplifyVector = TRUE)
  if (from_aggregated) {
    samples_csv <- sprintf(
      "out/replicate-%d/aggregated-posterior-samples.csv",
      replicate_ix
    )
    header <- c("llhd", "birth_rate", "rho_prob", "nu_prob", "nb_r", "nb_p")
  } else {
    samples_csv <- sprintf(
      "out/replicate-%d/posterior-samples.csv",
      replicate_ix
    )
    header <- c("llhd", "birth_rate", "sampling_rate", "omega_rate", "nb_r", "nb_p")
  }

  mcmc_df <- read.csv(file = samples_csv, header = FALSE, skip = num_burnin)
  names(mcmc_df) <- header
  if (!from_aggregated) {
    mcmc_df$r_naught <- mcmc_df$birth_rate / (death_rate + mcmc_df$sampling_rate + mcmc_df$omega_rate)
  }

  mcmc_df$prevalence <- rnbinom(
    n = nrow(mcmc_df),
    size = mcmc_df$nb_r,
    prob = 1 - mcmc_df$nb_p # because R uses 1-p parameterisation.
  )

  mcmc_obj <- as.mcmc(mcmc_df)

  quantile_df <- as.data.frame(summary(mcmc_obj)$quantiles)
  quantile_df$variable <- row.names(quantile_df)
  row.names(quantile_df) <- NULL
  quantile_df <- melt(quantile_df,
                      id.vars = "variable",
                      variable.name = "quantile")

  return(list(replicate = replicate_ix,
              final_prevalence = final_prev,
              estimates = quantile_df,
              diagnostics = list(effective_size = as.list(effectiveSize(mcmc_obj)))))
}

main <- function(args) {
  death_rate <- jsonlite::read_json(args$parameters)$deathRate
  summaries_not_agg <- list()
  summaries_agg <- list()

  for (replicate in seq.int(args$num_replicates)) {
    summaries_not_agg <- c(summaries_not_agg,
                           list(read_and_summarise(replicate, args$burnin, FALSE, death_rate)))
    summaries_agg <- c(summaries_agg,
                       list(read_and_summarise(replicate, args$burnin, TRUE, death_rate)))
  }
  write_summary(summaries_not_agg, summaries_agg, args$parameters, args$output)
}

if (!interactive()) {
  args <- parser$parse_args()
} else {
  args <- list(num_replicates = 30,
               parameters = "../example-parameters.json",
               burnin = 0,
               output = "out/foo-summary.json")
}
main(args)
