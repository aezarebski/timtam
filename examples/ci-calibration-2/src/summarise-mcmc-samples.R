library(argparse)
library(coda)
library(reshape2)
library(dplyr)
library(magrittr)

## Set the seed because there is re-sampling involved in the estimation of the
## coverage of the prevalence CI.
set.seed(1)

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

  mses <- list(
    aggregated = do.call(rbind,
                         lapply(summaries_from_agg,
                                function(s) {
                                  data.frame(size = s$dataset_size,
 mse_prevalence = s$mse$prevalence)
                                })),
    not_aggregated = do.call(rbind,
                             lapply(summaries,
                                    function(s) {
                                      data.frame(size = s$dataset_size,
                                                 mse_r_naught = s$mse$r_naught,
                                                 mse_prevalence = s$mse$prevalence)
                                    }))
  )

  result <- list(prevalence = prevalences,
                 estimates = estimates,
                 mses = mses,
                 diagnostics = list(effectiveSize = list(aggregated = diags_agg, not_aggregated = diags_not_agg)))

  jsonlite::write_json(
              x = result,
              path = output,
              auto_unbox = T,
              pretty = TRUE
            )
}

## read the MCMC samples and return some summary statistics
read_and_summarise <- function(replicate_ix, from_aggregated, death_rate, r_naught) {
  data_dir <- sprintf("out/replicate-%d", replicate_ix)

  final_prev <- jsonlite::read_json(sprintf(
                            "%s/ape-sim-final-prevalence.json",
                            data_dir
                          ), simplifyVector = TRUE)
  if (from_aggregated) {
    samples_csv <- sprintf(
      "%s/aggregated-posterior-samples.csv",
      data_dir
    )
    header <- c("llhd", "birth_rate", "rho_prob", "nu_prob", "nb_r", "nb_p")
    dataset_size <- jsonlite::read_json(sprintf("%s/mcmc-app-config-aggregated.json", data_dir)) |> extract2("mcmcObservations") |> length()
  } else {
    samples_csv <- sprintf(
      "out/replicate-%d/posterior-samples.csv",
      replicate_ix
    )
    header <- c("llhd", "birth_rate", "sampling_rate", "omega_rate", "nb_r", "nb_p")
    dataset_size <- jsonlite::read_json(sprintf("%s/mcmc-app-config.json", data_dir)) |> extract2("mcmcObservations") |> length()
  }

  mcmc_df <- tryCatch(
    read.csv(file = samples_csv, header = FALSE),
    error = function(e) {
      warning(e)
      return(NULL)
    }
  )
  if (is.null(mcmc_df)) {
    cat("\tcould not read mcmc samples so terminating early.\n")
    return(NULL)
  }

  names(mcmc_df) <- header
  if (!from_aggregated) {
    mcmc_df$r_naught <- mcmc_df$birth_rate / (death_rate + mcmc_df$sampling_rate + mcmc_df$omega_rate)
    mse_r_naught <- mean((mcmc_df$r_naught - r_naught) ^ 2)
  } else {
    mse_r_naught <- NULL
  }

  mcmc_df$prevalence <- rnbinom(
    n = nrow(mcmc_df),
    size = mcmc_df$nb_r,
    prob = 1 - mcmc_df$nb_p # because R uses 1-p parameterisation.
  )

  mse <- list(
    prevalence = mean(((mcmc_df$prevalence - final_prev) / final_prev) ^ 2),
    r_naught = mse_r_naught
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
              mse = mse,
              dataset_size = dataset_size,
              diagnostics = list(effective_size = as.list(effectiveSize(mcmc_obj)))))
}

main <- function(args) {
  true_params <- jsonlite::read_json(args$parameters)
  death_rate <- true_params$deathRate
  true_r_naught <- true_params$birthRate / (true_params$deathRate + true_params$samplingRate + true_params$occurrenceRate)
  summaries_not_agg <- list()
  summaries_agg <- list()

  for (replicate in seq.int(args$num_replicates)) {
    cat("Processing replicate ", replicate, "\n")
    summary_not_agg <- read_and_summarise(replicate, FALSE, death_rate, true_r_naught)
    summary_agg <- read_and_summarise(replicate, TRUE, death_rate, true_r_naught)
    if (!any(sapply(list(summary_not_agg, summary_agg), is.null))) {
      summaries_not_agg <- c(summaries_not_agg,
                             list(summary_not_agg))
      summaries_agg <- c(summaries_agg,
                         list(summary_agg))
    } else {
      cat("skipping replicate ", replicate, "\n")
    }
  }
  write_summary(summaries_not_agg, summaries_agg, args$parameters, args$output)
}

if (!interactive()) {
  args <- parser$parse_args()
} else {
  args <- list(num_replicates = 20,
               parameters = "../example-parameters.json",
               output = "out/foo-summary.json")
}
main(args)
