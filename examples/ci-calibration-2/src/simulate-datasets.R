suppressPackageStartupMessages(library("argparse"))

# create parser object
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
         "-d",
         "--duration",
         type = "double",
         help = "Simulation duration"
       )
parser$add_argument(
         "--seq-agg-times",
         type = "character",
         default = "",
         help = "Specification of aggregation times for sequenced samples: \"FROM TO BY\". These values get read as three numbers then form the arguments for the seq function.")
parser$add_argument(
         "--occ-agg-times",
         type = "character",
         default = "",
         help = "Specification of aggregation times for unsequenced samples (occurrence data). See the details of --seq-agg-times.")
parser$add_argument(
         "--num-mcmc-samples",
         type = "integer",
         help = "The number of posterior samples to generate via MCMC."
       )
parser$add_argument(
         "--burn",
         type = "integer",
         help = "The number of MCMC samples to discard from the start of the chain."
       )
parser$add_argument(
         "--thin",
         type = "integer",
         help = "The factor to thin the MCMC samples by."
       )

#' Helper function for running system commands.
#'
#' Example
#' > run_command("stack build")
#' > run_command("Rscript R/make-simulation-configuration-1.R")
#'
run_command <- function(cmd, silent = TRUE, ...) {
  message(sprintf("\nRunning command:\n\t%s", cmd))
  tmp <- unlist(strsplit(cmd, " "))
  cmd_name <- head(tmp, 1)
  cmd_args <- tail(tmp, -1)
  std_val <- if (silent) {
               FALSE
             } else {
               ""
             }
  result <- system2(
    cmd_name,
    args = cmd_args,
    stdout = std_val,
    stderr = std_val,
    ...
  )
  if (result != 0) {
    stop(sprintf("command %s failed!", cmd))
  }
  return(NULL)
}

replicate_dir <- function(seed) {
  sprintf("out/replicate-%d", seed)
}

run_simulation <- function(num_replicates, duration) {
  sim_cmd <- function(seed) {
    sprintf(
      "../ape-simulation/ape-sim.R --seed %d -p ../example-parameters.json -o %s --duration %f --make-plots -v --seq-agg-times=\"0 %f 1\" --occ-agg-times=\"0.5 %f 7\"",
      seed,
      replicate_dir(seed),
      duration,
      duration,
      duration
    )
  }
  for (seed in seq.int(num_replicates)) {
    dir.create(replicate_dir(seed))
    run_command(sim_cmd(seed))
  }
}

prepare_data <- function(num_replicates, num_mcmc_samples, num_burn, thinning_factor) {
  prep_cmd <- function(seed, is_aggregated) {
    input_csv <- sprintf("%s/%s",
                         replicate_dir(seed),
                         ifelse(is_aggregated,
                                "ape-sim-aggregated-event-times.csv",
                                "ape-sim-event-times.csv")
                         )
    output_json <- sprintf("%s/%s",
                           replicate_dir(seed),
                           ifelse(is_aggregated,
                                  "mcmc-app-config-aggregated.json",
                                  "mcmc-app-config.json"))
    param_str <- ifelse(is_aggregated,
                        "\"identity-muKnown-lambda-psiZero-rho-omegaZero-nu\"",
                        "\"identity-muKnown-lambda-psi-noRho-omega-noNu\"")
    mcmc_str <- sprintf("--num-mcmc-samples %d --burn %d --thin %d",
                        num_mcmc_samples,
                        num_burn,
                        thinning_factor)
    sprintf(
      "Rscript src/make-config.R --input %s --output %s --parameterisation=%s %s",
      input_csv,
      output_json,
      param_str,
      mcmc_str
    )
  }
  for (seed in seq.int(num_replicates)) {
    run_command(prep_cmd(seed, FALSE))
    run_command(prep_cmd(seed, TRUE))
  }
}

main <- function(args) {
  cat("hello from simulate-datasets.R\n")
  run_simulation(args$num_replicates, args$duration)
  prepare_data(args$num_replicates, args$num_mcmc_samples, args$burn, args$thin)
}

if (!interactive()) {
  args <- parser$parse_args()
  main(args)
}
